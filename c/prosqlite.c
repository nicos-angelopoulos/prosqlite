#include <sqlite3.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdbool.h>

atom_t row_atom;
functor_t minus2_functor;

static atom_t ATOM_true;
static atom_t ATOM_false;

typedef struct sqlite_blob_rec
{ sqlite3  *dbh;
  // int       bl_val;
  atom_t    symbol;  // this makes the rec aware of its blob handle (i think), see acquire_sqlite_blob()
} sqlite_blob_rec;

// int PL_SQLite_Connection_release(atom_t connection)
// {
  // printf("release\n");
  // return 1;
// }

static int
release_sqlite_blob(atom_t symbol)
{ sqlite_blob_rec *blr = PL_blob_data(symbol, NULL, NULL );
	PL_free(blr);
	return TRUE;
}

static int
compare_sqlite_blob(atom_t a, atom_t b)
{ sqlite_blob_rec *ara = PL_blob_data(a, NULL, NULL);
  sqlite_blob_rec *arb = PL_blob_data(b, NULL, NULL);
  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}

static int
write_sqlite_blob(IOSTREAM *s, atom_t symbol, int flags)
{ sqlite_blob_rec *blr= PL_blob_data(symbol, NULL, NULL);
  Sfprintf(s, "<sqlite>(%p)", blr);
  return TRUE;
}

static void
acquire_sqlite_blob(atom_t symbol)
{ sqlite_blob_rec *blr = PL_blob_data(symbol, NULL, NULL);
  blr->symbol = symbol;
}

PL_blob_t PL_SQLite_Connection = {
  PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE | PL_BLOB_NOCOPY,
  "sqlite",              // "SQLiteConnection",
  release_sqlite_blob,  // PL_SQLite_Connection_release, // release
  compare_sqlite_blob,  // 0, // compare
  write_sqlite_blob,    // write
  acquire_sqlite_blob   // acquire
};

static foreign_t c_sqlite_connect(term_t filename, term_t connection)
{
  int rc;
  char *filename_c;

  PL_STRINGS_MARK();
  if ( (rc=PL_get_file_name(filename, &filename_c, PL_FILE_OSPATH)) )
  {
    sqlite_blob_rec *crec;
    sqlite3* handle;

    if (sqlite3_open(filename_c, &handle) == SQLITE_OK)
    {
      crec = calloc(1,sizeof(*crec));
      crec->dbh = handle;
      rc = PL_unify_blob(connection, crec, sizeof(*crec),
			 &PL_SQLite_Connection);
    } else {
      rc = PL_permission_error("open", "sqlite_db", filename);
    }
  }
  PL_STRINGS_RELEASE();

  return rc;
}


static foreign_t c_sqlite_disconnect(term_t connection)
{
  sqlite3* db;
  sqlite_blob_rec *conn;

  if (PL_get_blob(connection, (void**)&conn, 0, 0))
   db = conn->dbh;
  {
    if (sqlite3_close_v2(db) == SQLITE_OK)  // 16.08.27, changed from sqlite3_close(db) - Christian
      {
         PL_succeed;
      }  else {
         printf("SQLite returned error at closing database \n");
         PL_fail;
         };
  }

  printf("could not get connection to close \n");
  PL_fail;
}


typedef struct query_context
{
  sqlite3_stmt* statement;
  functor_t row_functor;
  int num_columns;
  int* column_types;
} query_context;


query_context* new_query_context(sqlite3_stmt* statement)
{
  query_context* context = (query_context*)malloc(sizeof(query_context));
  context->num_columns = sqlite3_column_count(statement);
  context->row_functor = PL_new_functor(row_atom, context->num_columns);
  context->statement = statement;

  context->column_types = (int*)malloc(sizeof(int) * context->num_columns);
  for (int i = 0; i < context-> num_columns; i++)
    context->column_types[i] = sqlite3_column_type(statement, i);

  return context;
}


void free_query_context(query_context* context)
{
  sqlite3_finalize(context->statement);
  free(context->column_types);
  free(context);
}

int unify_row_term(term_t row, query_context* context)
{
  if (!PL_unify_functor(row, context->row_functor))
    PL_fail;

    // printf("text: %d \n", SQLITE_TEXT);
    // printf("null: %d \n", SQLITE_NULL);

  for (int i = 0; i < context->num_columns; i++)
  {
    term_t col_term = PL_new_term_ref();

    if ( sqlite3_column_type(context->statement,i) == SQLITE_NULL)
      {
			if (!PL_put_atom_chars(col_term, "$null$" ))
         return FALSE;
         // fixme: what happens at else ? 
      }
      else
      {
    switch (context->column_types[i])
    {
    case SQLITE_INTEGER:
      if ( !PL_put_integer(col_term, sqlite3_column_int(context->statement, i)) )
	return FALSE;
      break;

    case SQLITE_FLOAT:
      if ( !PL_put_float(col_term, sqlite3_column_double(context->statement, i)) )
	return FALSE;
      break;


    case SQLITE_TEXT:
				 if (sqlite3_column_bytes(context->statement,i))
					// PL_put_atom_chars(col_term, sqlite3_column_text(context->statement, i));
					// Samer's fix (1/2)
					 {   // block this as else below is meant for the outer if
					   if (!PL_unify_chars(col_term, PL_ATOM | REP_UTF8, (size_t)-1, (char*)sqlite3_column_text(context->statement, i)))
                  return FALSE;
					 }
				else
					// this should probably never be the case (should be SQLITE_NULL) but firefox's places.sqlite
					// has 0 length texts
                  {
					   PL_put_atom_chars(col_term, "" );
               }

      break;

    case SQLITE_BLOB:
      // TODO: what prolog type should BLOBs be mapped to?
      // PL_put_blob?
		// for now map to string, as you can later can access the bytes
		// Samer's fix 2/2:
	if (sqlite3_column_bytes(context->statement,i)) {
	  if (!PL_unify_chars(col_term, PL_STRING | REP_UTF8, (size_t)-1, (char*)sqlite3_column_text(context->statement, i))) return FALSE;
      } else {
    if (!PL_put_string_chars(col_term, "")) return FALSE;
		}
      break;

    case SQLITE_NULL:
      // TODO: what about this?
      // PL_put_nil?
		 if (sqlite3_column_bytes(context->statement,i))
					// this should probably never be the case (should be SQLITE_TEXT?) but firefox's places.sqlite
					// has non 0 length nulls
					PL_put_atom_chars(col_term, (char*)sqlite3_column_text(context->statement, i));
				else
					// PL_put_nil(col_term)  // would be more correct probably
					PL_put_atom_chars(col_term, "$null$" );
      break;

    }
   }

    if (!PL_unify_arg(i + 1, row, col_term))
      PL_fail;
  }

  PL_succeed;
}

static foreign_t c_sqlite_version(term_t ver, term_t datem)
{
    term_t tmp = PL_new_term_ref();
	 // 1:0:0,  date(2014,12,23)
    if ( PL_unify_term(tmp,PL_FUNCTOR_CHARS,":",2,PL_INT, 4, PL_INT, 0) &&    // Minor + Fix
         PL_unify_term(ver,PL_FUNCTOR_CHARS,":",2,PL_INT, 1, PL_TERM, tmp ) &&   // Major
         PL_unify_term(datem,PL_FUNCTOR_CHARS,"date",3,PL_INT, 2018, PL_INT, 3, PL_INT, 17) )
         // 1:1:0, 2016,10,9
         // 1:3:0, 2018,3,17,  proper support for blobs (see examples/two.pl)
         // 1:4:0, 2024,8,14
      return TRUE;
      else
      return FALSE;

    return FALSE;
    // PL_unify_term(ver,PL_FUNCTOR_CHARS,":",1,PL_CHARS,
}

static foreign_t c_library_version(term_t ver)
{
      if ( PL_unify_term(ver,PL_CHARS,sqlite3_libversion()) )
      return TRUE;
      else
      return FALSE;

    return FALSE;
    // PL_unify_term(ver,PL_FUNCTOR_CHARS,":",1,PL_CHARS,
}

static foreign_t c_build_version(term_t ver)
{
      if ( PL_unify_term(ver,PL_CHARS,SQLITE_VERSION) )
      return TRUE;
      else
      return FALSE;
    return FALSE;
}

int raise_sqlite_exception(sqlite3* db)
{
  term_t except = PL_new_term_ref();
  if ( PL_unify_term(except,
		     PL_FUNCTOR_CHARS, "sqlite_error", 2,
		       PL_INT, sqlite3_errcode(db),
		       PL_CHARS, sqlite3_errmsg(db)) )
    return PL_raise_exception(except);

  return FALSE;
}

// Copied and adapted from the odbc package
int formatted_string(term_t in, char** out)
{
  term_t av = PL_new_term_refs(3);
  static predicate_t format;
  //char *out = NULL;
  size_t len = 0;
  *out = NULL;
  IOSTREAM *fd = Sopenmem(out, &len, "w");

  if (!fd)
    return FALSE;                       /* resource error */
  if (!format)
    format = PL_predicate("format", 3, "user");

  if (!PL_unify_stream(av+0, fd) ||
      !PL_get_arg(1, in, av+1) ||
      !PL_get_arg(2, in, av+2) ||
      !PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, format, av))
  {
    Sclose(fd);
    if (*out)
      PL_free(*out);
    return FALSE;
  }
  Sclose(fd);

    if (*out)
      PL_free(*out);

  return TRUE;
}


int get_query_string(term_t tquery, char** query)
{
  if (PL_is_functor(tquery, minus2_functor))
    return formatted_string(tquery, query);
  else
    return PL_get_chars(tquery, query, CVT_ATOM|CVT_STRING|BUF_MALLOC|REP_UTF8);
}


// Jan says "You must call PL_free() on the query strings after you are done with them!"
// fixme: check here or at Prolog that Connection/Alias IS open: otherwise
//                        we get sqlite_error(21,library routine called out of sequence)
static foreign_t c_sqlite_query(term_t connection, term_t query, term_t row,
				 control_t handle)
{
  sqlite3* db;
  sqlite_blob_rec *conn;

  // conn = &connection;

  // db = connection->dbh;

  query_context* context;
  term_t tmp = PL_new_term_ref();
  int changes = 0;

  switch (PL_foreign_control(handle))
  {
  case PL_FIRST_CALL:
    PL_get_blob(connection, (void**)&conn, 0, 0);
   db = conn->dbh;

    char* query_c;
    sqlite3_stmt* statement;
    if (!get_query_string(query, &query_c))
      {
         PL_free(query_c);
         PL_fail;
      };

    if (sqlite3_prepare(db, query_c, -1, &statement, 0) != SQLITE_OK)
      { PL_free(query_c);
        return raise_sqlite_exception(db);
      }

    PL_free(query_c);

    bool recurrent = false;
    switch (sqlite3_step(statement))
    {
    case SQLITE_ROW:
      context = new_query_context(statement);
      recurrent = true;
      if ( unify_row_term(row, context) )
	PL_retry_address(context);
      /*FALLTHROUGH*/
    case SQLITE_DONE:
       if (recurrent)
         {
            free_query_context(context);
            PL_fail;
         } else
         {

            int what = sqlite3_column_count(statement);
            if (what) {    /* >0 means statement is supposed to return results */
               sqlite3_finalize(statement);
               PL_fail;
            }
               else   {    /* statement is a INSERT/DELETE/UPDATE which do not return anything */
                     context = new_query_context(statement);
                     changes = sqlite3_changes(db);
                     if (PL_unify_term(tmp,PL_FUNCTOR_CHARS,"row",1,PL_INT64, (int)changes))
                        if( !PL_unify(row,tmp) )
                           { free_query_context(context);
                              PL_fail;}
                     free_query_context(context);
               PL_succeed;
            };
         }
     default:
     return raise_sqlite_exception(db);
    }

  case PL_REDO:

    context = PL_foreign_context_address(handle);
    switch (sqlite3_step(context->statement))
    {
    case SQLITE_ROW:
      if ( unify_row_term(row, context) )
	PL_retry_address(context);
      /*FALLTHROUGH*/
    case SQLITE_DONE:
      free_query_context(context);
      PL_fail;
    }

  case PL_PRUNED:
    context = PL_foreign_context_address(handle);
    free_query_context(context);
    PL_succeed;
  }

  PL_fail;
}


install_t install_prosqlite()
{

  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");

  row_atom = PL_new_atom("row");
  minus2_functor = PL_new_functor(PL_new_atom("-"), 2);
  PL_register_foreign("c_sqlite_version", 2, c_sqlite_version, 0);
  PL_register_foreign("c_library_version", 1, c_library_version, 0);
  PL_register_foreign("c_build_version", 1, c_build_version, 0);
  PL_register_foreign("c_sqlite_connect", 2, c_sqlite_connect, 0);
  PL_register_foreign("c_sqlite_disconnect", 1, c_sqlite_disconnect, 0);
  PL_register_foreign("c_sqlite_query", 3, c_sqlite_query,
		      PL_FA_NONDETERMINISTIC);
}


install_t uninstall_prosqlite()
{
  PL_unregister_atom(row_atom);
  PL_unregister_blob_type(&PL_SQLite_Connection);
}

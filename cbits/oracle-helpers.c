#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dpi.h"

int context_create (int majorVersion, int minorVersion, dpiContext **context, dpiErrorInfo *errorInfo) {
  return dpiContext_createWithParams (majorVersion, minorVersion, NULL, context, errorInfo);
}


#define USER           "j222933"
#define PASSWORD       "OoEHstuj"
#define CONNECT_STRING "wpx4-scan.heb.com:1521/pdb2om1_rw"

static dpiContext *gContext = NULL;
static dpiErrorInfo gErrorInfo;

int printError(void)
{
    if (gContext)   dpiContext_getError(gContext, &gErrorInfo);
    fprintf(stderr, " [FAILED]\n");
    fprintf(stderr, "    FN: %s\n", gErrorInfo.fnName);
    fprintf(stderr, "    ACTION: %s\n", gErrorInfo.action);
    fprintf(stderr, "    MSG: %.*s\n", gErrorInfo.messageLength, gErrorInfo.message);
    fflush(stderr);
    return DPI_FAILURE;
}

int getMajorVersion () {
  return DPI_MAJOR_VERSION;
}

int getMinorVersion () {
  return DPI_MINOR_VERSION;
}

int main () {
    printf("data -> %zu\n", sizeof(dpiData)); // 40 bytes
    printf("data buffer -> %zu\n", sizeof(dpiDataBuffer)); // 48 bytes

    printf("ts -> %zu\n", sizeof(dpiTimestamp));
    printf("int16_t -> %zu\n", sizeof(int16_t));
    printf("uint8_t -> %zu\n", sizeof(uint8_t));


    const char *selectSql = "select count(*) from EMD.item_master";
    dpiConn *conn;
    dpiStmt *stmt;
    int found;
    uint32_t bufferRowIndex;

    dpiData *tsColValue;
    dpiNativeTypeNum nativeTypeNum;

    printf("major -> %d\n", DPI_MAJOR_VERSION);
    printf("minor -> %d\n", DPI_MINOR_VERSION);

    // create context
    if (dpiContext_create(DPI_MAJOR_VERSION, DPI_MINOR_VERSION, &gContext, &gErrorInfo) < 0) {
      printError();
      return 1;
    }

    // create connection
    if (dpiConn_create(gContext, USER, strlen(USER), PASSWORD, strlen(PASSWORD),
		       CONNECT_STRING, strlen(CONNECT_STRING), NULL, NULL, &conn) < 0) {
      printError();
      return 1;
    }

    // prepare and bind insert statement
    if (dpiConn_prepareStmt(conn, 0, selectSql, strlen(selectSql), NULL, 0, &stmt) < 0) {
      printError();
      return 1;
    }

    // exec that shiii
    if (dpiStmt_execute(stmt, DPI_MODE_EXEC_DEFAULT, NULL) < 0) {
      printError ();
      return 1;
    }

    // fetch rows
    while (1) {
        if (dpiStmt_fetch(stmt, &found, &bufferRowIndex) < 0)
            return printError();

        if (!found)
            break;

	printf("bufferRowIndex -> %d\n", bufferRowIndex);

        if (dpiStmt_getQueryValue(stmt, 1, &nativeTypeNum, &tsColValue) < 0)
            return printError();

	printf("type -> %d\n", nativeTypeNum);

        printf( "%f\n"
	       , tsColValue->value.asDouble
	       );
	}


    // cleanup
    dpiStmt_release(stmt);
    dpiConn_release(conn);

    printf("Done\n");
    return 0;
 }

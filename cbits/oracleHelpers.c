#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dpi.h"

int context_create (int majorVersion, int minorVersion, dpiContext **context, dpiErrorInfo *errorInfo) {
  return dpiContext_createWithParams (majorVersion, minorVersion, NULL, context, errorInfo);
}

void finalize_connection_default (dpiConn *conn) {
  dpiConn_close (conn, DPI_MODE_CONN_CLOSE_DEFAULT, NULL, 0);
}

void close_pool_default (dpiPool *pool) {
  dpiPool_close (pool, DPI_MODE_POOL_CLOSE_DEFAULT);
}

int getMajorVersion () {
  return DPI_MAJOR_VERSION;
}

int getMinorVersion () {
  return DPI_MINOR_VERSION;
}

int acquire_connection (dpiPool *pool, dpiConn **conn) {
  return dpiPool_acquireConnection (pool, NULL, 0, NULL, 0, NULL, conn);
}

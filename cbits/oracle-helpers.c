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

int getMajorVersion () {
  return DPI_MAJOR_VERSION;
}

int getMinorVersion () {
  return DPI_MINOR_VERSION;
}


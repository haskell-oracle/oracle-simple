#include "dpi.h"

/*
 * Helper functions that allow us to access members of the dpiDataBuffer union.
 */

dpiJsonObject *dpiDataBuffer_getAsJsonObject(dpiDataBuffer *buffer)
{
  return &buffer->asJsonObject;
}

dpiJsonArray *dpiDataBuffer_getAsJsonArray(dpiDataBuffer *buffer)
{
  return &buffer->asJsonArray;
}

dpiBytes *dpiDataBuffer_getAsBytes(dpiDataBuffer *buffer)
{
  return &buffer->asBytes;
}

int dpiDataBuffer_getAsBoolean(dpiDataBuffer *buffer)
{
  return buffer->asBoolean;
}

double dpiDataBuffer_getAsDouble(dpiDataBuffer *buffer)
{
  return buffer->asDouble;
}

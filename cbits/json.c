#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dpi.h"

#define SQL     "select" \
                "    json_array(" \
                "        json_scalar(1)," \
                "        json_scalar('String value')," \
                "        json_object(" \
                "            key 'Fred' value json_scalar(5.12)," \
                "            key 'George' value json_scalar('A string')" \
                "        returning json)," \
                "        json_array(" \
                "            json_scalar(utl_raw.cast_to_raw('A raw'))," \
                "            json_scalar(sysdate - 1)," \
                "            json_scalar(systimestamp + 1)" \
                "        returning json)" \
                "    returning json) " \
                "from dual"

// #define SQL "select * from json_demo"

void displayJson(dpiJsonNode *node, FILE *jsonOut)
{
    dpiTimestamp *timestamp;
    dpiJsonArray *array;
    dpiJsonObject *obj;
    uint32_t i;

    switch (node->nativeTypeNum) {

        /* json 'object' type */
        case DPI_NATIVE_TYPE_JSON_OBJECT:
            obj = &node->value->asJsonObject;
            fprintf(jsonOut, "{");
            for (i = 0; i < obj->numFields; i++) {
                if (i > 0)
                    fprintf(jsonOut, ",");
                fprintf(jsonOut, "\"%.*s\": ", obj->fieldNameLengths[i],
                        obj->fieldNames[i]);
                displayJson(&obj->fields[i], jsonOut);
            }
            fprintf(jsonOut, "}");
            break;

        /* json 'array' type */
        case DPI_NATIVE_TYPE_JSON_ARRAY:
            array = &node->value->asJsonArray;
            fprintf(jsonOut, "[");
            for (i = 0; i < array->numElements; i++) {
                if (i > 0)
                    fprintf(jsonOut, ",");
                displayJson(&array->elements[i], jsonOut);
            }
            fprintf(jsonOut, "]");
            break;

        /* json 'string' type */
        case DPI_NATIVE_TYPE_BYTES:
            fprintf(jsonOut, "\"%.*s\"", node->value->asBytes.length,
                    node->value->asBytes.ptr);
            break;

        /* json 'number' type */
        case DPI_NATIVE_TYPE_DOUBLE:
            fprintf(jsonOut, "%g", node->value->asDouble);
            break;

        /* json 'true' and 'false' literals */
        case DPI_NATIVE_TYPE_BOOLEAN:
            fprintf(jsonOut, "%s", node->value->asBoolean ? "true" : "false");
            break;
        
        /* json 'null' literal */
        case DPI_NATIVE_TYPE_NULL:
            fprintf(jsonOut, "null");
            break;

        /*
         * non-standard timestamp type!
         * not a part of ECMA-404, in which timestamps are represented as strings
         * we format timestamps as ISO-8601 since that's the closest to a JSON standard
         */
        case DPI_NATIVE_TYPE_TIMESTAMP:
            timestamp = &node->value->asTimestamp;
            fprintf(jsonOut, "%.4d-%.2d-%.2d %.2d:%.2d:%.2d", timestamp->year,
                    timestamp->month, timestamp->day, timestamp->hour,
                    timestamp->minute, timestamp->second);
            break;

        default:
            printf("Unhandled native type %u", node->nativeTypeNum);
    }
}

void displayJsonRunner(dpiJsonNode *node, char **jsonStr) {

  FILE *jsonOut = tmpfile();
  displayJson(node, jsonOut);
  long length;

  char *buffer = 0;

  if(jsonOut) {
    fseek(jsonOut, 0, SEEK_END);
    length = ftell(jsonOut);
    fseek(jsonOut, 0, SEEK_SET);
    buffer = malloc(length);
    if (buffer) {
      fread(buffer, 1, length, jsonOut);
    }
    fclose(jsonOut); // deletes temp file
  }

  *jsonStr = buffer;
}

int run_json_demo(dpiConn *conn, char **jsonStr)
{
    dpiNativeTypeNum nativeTypeNum;
    uint32_t bufferRowIndex;
    dpiData *jsonColValue;
    dpiJsonNode *topNode;
    dpiStmt *stmt;
    int found;

    // perform query
    if (dpiConn_prepareStmt(conn, 0, SQL, strlen(SQL), NULL, 0, &stmt) < 0) {
        printf("Oops!\n");
        return -1;
    }
    if (dpiStmt_execute(stmt, DPI_MODE_EXEC_DEFAULT, NULL) < 0) {
        printf("Oops!\n");
        return -1;
    }

    // fetch row
    if (dpiStmt_fetch(stmt, &found, &bufferRowIndex) < 0) {
        printf("Oops!\n");
        return -1;
    }
    if (dpiStmt_getQueryValue(stmt, 1, &nativeTypeNum, &jsonColValue) < 0) {
        printf("Oops!\n");
        return -1;
    }
    if (dpiJson_getValue(jsonColValue->value.asJson, DPI_JSON_OPT_DEFAULT,
            &topNode) < 0) {
        printf("Oops!\n");
        return -1;
    }


    // display results
    printf("JSON value:\n");
    displayJsonRunner(topNode, jsonStr);
    printf("\n");

    return 0;
}

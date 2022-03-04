#pragma once

#include "rocksdb/c.h"

extern ROCKSDB_LIBRARY_API void rocksdb_delete_range(rocksdb_t* db,
                          const rocksdb_writeoptions_t* options,
                          const char* start_key, size_t start_key_len,
                          const char* end_key, size_t end_key_len,
                          char** errptr);

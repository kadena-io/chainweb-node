//  Copyright (c) 2011-present, Facebook, Inc.  All rights reserved.
//  This source code is licensed under both the GPLv2 (found in the
//  COPYING file in the root directory) and Apache 2.0 License
//  (found in the LICENSE.Apache file in the root directory).
//
// Copyright (c) 2011 The LevelDB Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file. See the AUTHORS file for names of contributors.

// The code not taken from the LevelDB and RocksDB authors is 
// Copyright (c) 2022 Kadena.

// copied from rocksdb/c.cc
#include <stdlib.h>
#include "rocksdb/db.h"

#include <vector>
#include <unordered_set>
#include <map>

using rocksdb::DB;
using rocksdb::DBOptions;
using rocksdb::DbPath;
using rocksdb::Env;
using rocksdb::EnvOptions;
using rocksdb::Slice;
using rocksdb::SliceParts;
using rocksdb::SliceTransform;
using rocksdb::Snapshot;
using rocksdb::SstFileWriter;
using rocksdb::Status;
using rocksdb::WritableFile;
using rocksdb::WriteOptions;

using std::shared_ptr;
using std::vector;
using std::unordered_set;
using std::map;

extern "C" {

struct rocksdb_t                 { DB*               rep; };
struct rocksdb_writeoptions_t    { WriteOptions      rep; };

static bool SaveError(char** errptr, const Status& s) {
  assert(errptr != nullptr);
  if (s.ok()) {
    return false;
  } else if (*errptr == nullptr) {
    *errptr = strdup(s.ToString().c_str());
  } else {
    // TODO(sanjay): Merge with existing error?
    // This is a bug if *errptr is not created by malloc()
    free(*errptr);
    *errptr = strdup(s.ToString().c_str());
  }
  return true;
}
// end of code copied from rocksdb/c.cc

void rocksdb_delete_range(rocksdb_t* db,
                          const rocksdb_writeoptions_t* options,
                          const char* start_key, size_t start_key_len,
                          const char* end_key, size_t end_key_len,
                          char** errptr) {
      SaveError(errptr, db->rep->DeleteRange(options->rep, nullptr,
                                         Slice(start_key, start_key_len),
                                         Slice(end_key, end_key_len)));
}

}

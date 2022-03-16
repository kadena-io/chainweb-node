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
#include "rocksdb/slice_transform.h"

#include <vector>
#include <unordered_set>
#include <map>

using rocksdb::DB;
using rocksdb::DBOptions;
using rocksdb::DbPath;
using rocksdb::Env;
using rocksdb::EnvOptions;
using rocksdb::Options;
using rocksdb::ReadOptions;
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

class TablePrefixTransform : public SliceTransform {
 private:

  static const char* kClassName() { return "kadena.rocksdb.TablePrefix"; }
  static const char* kNickName() { return "table"; }
  const char* Name() const override { return kClassName(); }

  Slice Transform(const Slice& src) const override {
    size_t prefix_end;
    if ((prefix_end = std::string(src.data()).find("$")) != std::string::npos) {
      return Slice(src.data(), prefix_end);
    } else if ((prefix_end = std::string(src.data()).find("%")) != std::string::npos) {
      return Slice(src.data(), prefix_end);
    } else {
      // we must return the entire string if there's no symbol because of the law:
      // prefix(prefix(str)) == prefix(str)
      // this prevents us from implementing a real InDomain, which will go badly
      // (redundant prefix seeking)
      // *if* we mix prefixed and nonprefixed keys in the same database
      return src;
    }
  }

  bool InDomain(const Slice& src) const override {
    return true;
  }

  bool InRange(const Slice& dst) const override {
    return true;
  }

  bool FullLengthEnabled(size_t* len) const override {
    return false;
  }

  bool SameResultWhenAppended(const Slice& prefix) const override {
    return false;
  }
};

extern "C" {

struct rocksdb_t                 { DB*               rep; };
struct rocksdb_options_t         { Options           rep; };
struct rocksdb_writeoptions_t    { WriteOptions      rep; };
struct rocksdb_readoptions_t     { ReadOptions       rep; };

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

void rocksdb_readoptions_set_auto_prefix_mode(rocksdb_readoptions_t* options, bool auto_prefix_mode) {
  options->rep.auto_prefix_mode = auto_prefix_mode;
}

void rocksdb_options_set_dollar_denoted(rocksdb_options_t* options) {
  options->rep.prefix_extractor =
    std::make_shared<TablePrefixTransform>(TablePrefixTransform());
}

}
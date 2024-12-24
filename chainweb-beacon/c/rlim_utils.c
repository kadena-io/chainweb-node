#include <sys/resource.h>
#include <errno.h>
#include <sys/types.h>
#include <stdint.h>

struct uint64_t_pair {
  uint64_t fst;
  uint64_t snd;
};

int get_open_file_limits(struct uint64_t_pair* out) {
  struct rlimit lim;
  int ret;
  if ((ret = getrlimit(RLIMIT_NOFILE, &lim)) == 0) {
    out->fst = lim.rlim_cur;
    out->snd = lim.rlim_max;
  } else {
    out->fst = -1;
    out->snd = -1;
  }
  return ret;
}

int set_open_file_limits(struct uint64_t_pair *limits) {
  struct rlimit lim;
  lim.rlim_cur = limits->fst;
  lim.rlim_max = limits->snd;
  if (setrlimit(RLIMIT_NOFILE, &lim) == 0) return 0;
  else return errno;
}

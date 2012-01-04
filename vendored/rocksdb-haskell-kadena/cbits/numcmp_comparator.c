#include <stdlib.h>
#include <leveldb/c.h>

const char* numcmp_name(void* x)
{
        return "numcmp";
}

int numcmp_cmp(void* state, const char* a, size_t alen, const char* b, size_t blen)
{
        int al = atol(a);
        int bl = atol(b);
        if (al == bl)
              return 0;
        return al > bl ? 1 : -1;
}

void numcmp_destruct(void* x)
{
}

leveldb_comparator_t* numcmp_comparator(void)
{
        void *state = 0;
        return leveldb_comparator_create(
                state,
                numcmp_destruct,
                numcmp_cmp,
                numcmp_name
        );
}

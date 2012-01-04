#include <string.h>
#include <leveldb/c.h>

const char* strcmp_name(void* x)
{
        return "strcmp";
}

int strcmp_cmp(void* state, const char* a, size_t alen, const char* b, size_t blen)
{
        return strcmp(a, b);
}

void strcmp_destruct(void* x)
{
}

leveldb_comparator_t* strcmp_comparator(void)
{
        void *state = 0;
        return leveldb_comparator_create(
                state,
                strcmp_destruct,
                strcmp_cmp,
                strcmp_name
        );
}

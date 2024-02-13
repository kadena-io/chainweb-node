/* Lines 7-497 of this file are copied from the SQLite source code and are
 * subject to the respective copyright.
 *
 * Lines from 498 onward are copyright (c) 2022 Kadena LLC.
 */

/*
** 2017-03-08
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
******************************************************************************
**
** This SQLite extension implements functions that compute SHA3 hashes.
** Two SQL functions are implemented:
**
**     sha3(X,SIZE)
**     sha3_query(Y,SIZE)
**
** The sha3(X) function computes the SHA3 hash of the input X, or NULL if
** X is NULL.
**
** The sha3_query(Y) function evaluates all queries in the SQL statements of Y
** and returns a hash of their results.
**
** The SIZE argument is optional.  If omitted, the SHA3-256 hash algorithm
** is used.  If SIZE is included it must be one of the integers 224, 256,
** 384, or 512, to determine SHA3 hash variant that is computed.
*/
#include <sqlite3ext.h>
SQLITE_EXTENSION_INIT1

#include <assert.h>
#include <string.h>
#include <stdarg.h>

#ifndef SQLITE_AMALGAMATION
typedef sqlite3_uint64 u64;
#endif /* SQLITE_AMALGAMATION */

/******************************************************************************
** The Hash Engine
*/
/*
** Macros to determine whether the machine is big or little endian,
** and whether or not that determination is run-time or compile-time.
**
** For best performance, an attempt is made to guess at the byte-order
** using C-preprocessor macros.  If that is unsuccessful, or if
** -DSHA3_BYTEORDER=0 is set, then byte-order is determined
** at run-time.
*/
#ifndef SHA3_BYTEORDER
# if defined(i386)     || defined(__i386__)   || defined(_M_IX86) ||    \
     defined(__x86_64) || defined(__x86_64__) || defined(_M_X64)  ||    \
     defined(_M_AMD64) || defined(_M_ARM)     || defined(__x86)   ||    \
     defined(__arm__)
#   define SHA3_BYTEORDER    1234
# elif defined(sparc)    || defined(__ppc__)
#   define SHA3_BYTEORDER    4321
# else
#   define SHA3_BYTEORDER 0
# endif
#endif


/*
** State structure for a SHA3 hash in progress
*/
typedef struct SHA3Context SHA3Context;
struct SHA3Context {
  union {
    u64 s[25];                /* Keccak state. 5x5 lines of 64 bits each */
    unsigned char x[1600];    /* ... or 1600 bytes */
  } u;
  unsigned nRate;        /* Bytes of input accepted per Keccak iteration */
  unsigned nLoaded;      /* Input bytes loaded into u.x[] so far this cycle */
  unsigned ixMask;       /* Insert next input into u.x[nLoaded^ixMask]. */
};

/*
** A single step of the Keccak mixing function for a 1600-bit state
*/
static void KeccakF1600Step(SHA3Context *p){
  int i;
  u64 b0, b1, b2, b3, b4;
  u64 c0, c1, c2, c3, c4;
  u64 d0, d1, d2, d3, d4;
  static const u64 RC[] = {
    0x0000000000000001ULL,  0x0000000000008082ULL,
    0x800000000000808aULL,  0x8000000080008000ULL,
    0x000000000000808bULL,  0x0000000080000001ULL,
    0x8000000080008081ULL,  0x8000000000008009ULL,
    0x000000000000008aULL,  0x0000000000000088ULL,
    0x0000000080008009ULL,  0x000000008000000aULL,
    0x000000008000808bULL,  0x800000000000008bULL,
    0x8000000000008089ULL,  0x8000000000008003ULL,
    0x8000000000008002ULL,  0x8000000000000080ULL,
    0x000000000000800aULL,  0x800000008000000aULL,
    0x8000000080008081ULL,  0x8000000000008080ULL,
    0x0000000080000001ULL,  0x8000000080008008ULL
  };
# define a00 (p->u.s[0])
# define a01 (p->u.s[1])
# define a02 (p->u.s[2])
# define a03 (p->u.s[3])
# define a04 (p->u.s[4])
# define a10 (p->u.s[5])
# define a11 (p->u.s[6])
# define a12 (p->u.s[7])
# define a13 (p->u.s[8])
# define a14 (p->u.s[9])
# define a20 (p->u.s[10])
# define a21 (p->u.s[11])
# define a22 (p->u.s[12])
# define a23 (p->u.s[13])
# define a24 (p->u.s[14])
# define a30 (p->u.s[15])
# define a31 (p->u.s[16])
# define a32 (p->u.s[17])
# define a33 (p->u.s[18])
# define a34 (p->u.s[19])
# define a40 (p->u.s[20])
# define a41 (p->u.s[21])
# define a42 (p->u.s[22])
# define a43 (p->u.s[23])
# define a44 (p->u.s[24])
# define ROL64(a,x) ((a<<x)|(a>>(64-x)))

  for(i=0; i<24; i+=4){
    c0 = a00^a10^a20^a30^a40;
    c1 = a01^a11^a21^a31^a41;
    c2 = a02^a12^a22^a32^a42;
    c3 = a03^a13^a23^a33^a43;
    c4 = a04^a14^a24^a34^a44;
    d0 = c4^ROL64(c1, 1);
    d1 = c0^ROL64(c2, 1);
    d2 = c1^ROL64(c3, 1);
    d3 = c2^ROL64(c4, 1);
    d4 = c3^ROL64(c0, 1);

    b0 = (a00^d0);
    b1 = ROL64((a11^d1), 44);
    b2 = ROL64((a22^d2), 43);
    b3 = ROL64((a33^d3), 21);
    b4 = ROL64((a44^d4), 14);
    a00 =   b0 ^((~b1)&  b2 );
    a00 ^= RC[i];
    a11 =   b1 ^((~b2)&  b3 );
    a22 =   b2 ^((~b3)&  b4 );
    a33 =   b3 ^((~b4)&  b0 );
    a44 =   b4 ^((~b0)&  b1 );

    b2 = ROL64((a20^d0), 3);
    b3 = ROL64((a31^d1), 45);
    b4 = ROL64((a42^d2), 61);
    b0 = ROL64((a03^d3), 28);
    b1 = ROL64((a14^d4), 20);
    a20 =   b0 ^((~b1)&  b2 );
    a31 =   b1 ^((~b2)&  b3 );
    a42 =   b2 ^((~b3)&  b4 );
    a03 =   b3 ^((~b4)&  b0 );
    a14 =   b4 ^((~b0)&  b1 );

    b4 = ROL64((a40^d0), 18);
    b0 = ROL64((a01^d1), 1);
    b1 = ROL64((a12^d2), 6);
    b2 = ROL64((a23^d3), 25);
    b3 = ROL64((a34^d4), 8);
    a40 =   b0 ^((~b1)&  b2 );
    a01 =   b1 ^((~b2)&  b3 );
    a12 =   b2 ^((~b3)&  b4 );
    a23 =   b3 ^((~b4)&  b0 );
    a34 =   b4 ^((~b0)&  b1 );

    b1 = ROL64((a10^d0), 36);
    b2 = ROL64((a21^d1), 10);
    b3 = ROL64((a32^d2), 15);
    b4 = ROL64((a43^d3), 56);
    b0 = ROL64((a04^d4), 27);
    a10 =   b0 ^((~b1)&  b2 );
    a21 =   b1 ^((~b2)&  b3 );
    a32 =   b2 ^((~b3)&  b4 );
    a43 =   b3 ^((~b4)&  b0 );
    a04 =   b4 ^((~b0)&  b1 );

    b3 = ROL64((a30^d0), 41);
    b4 = ROL64((a41^d1), 2);
    b0 = ROL64((a02^d2), 62);
    b1 = ROL64((a13^d3), 55);
    b2 = ROL64((a24^d4), 39);
    a30 =   b0 ^((~b1)&  b2 );
    a41 =   b1 ^((~b2)&  b3 );
    a02 =   b2 ^((~b3)&  b4 );
    a13 =   b3 ^((~b4)&  b0 );
    a24 =   b4 ^((~b0)&  b1 );

    c0 = a00^a20^a40^a10^a30;
    c1 = a11^a31^a01^a21^a41;
    c2 = a22^a42^a12^a32^a02;
    c3 = a33^a03^a23^a43^a13;
    c4 = a44^a14^a34^a04^a24;
    d0 = c4^ROL64(c1, 1);
    d1 = c0^ROL64(c2, 1);
    d2 = c1^ROL64(c3, 1);
    d3 = c2^ROL64(c4, 1);
    d4 = c3^ROL64(c0, 1);

    b0 = (a00^d0);
    b1 = ROL64((a31^d1), 44);
    b2 = ROL64((a12^d2), 43);
    b3 = ROL64((a43^d3), 21);
    b4 = ROL64((a24^d4), 14);
    a00 =   b0 ^((~b1)&  b2 );
    a00 ^= RC[i+1];
    a31 =   b1 ^((~b2)&  b3 );
    a12 =   b2 ^((~b3)&  b4 );
    a43 =   b3 ^((~b4)&  b0 );
    a24 =   b4 ^((~b0)&  b1 );

    b2 = ROL64((a40^d0), 3);
    b3 = ROL64((a21^d1), 45);
    b4 = ROL64((a02^d2), 61);
    b0 = ROL64((a33^d3), 28);
    b1 = ROL64((a14^d4), 20);
    a40 =   b0 ^((~b1)&  b2 );
    a21 =   b1 ^((~b2)&  b3 );
    a02 =   b2 ^((~b3)&  b4 );
    a33 =   b3 ^((~b4)&  b0 );
    a14 =   b4 ^((~b0)&  b1 );

    b4 = ROL64((a30^d0), 18);
    b0 = ROL64((a11^d1), 1);
    b1 = ROL64((a42^d2), 6);
    b2 = ROL64((a23^d3), 25);
    b3 = ROL64((a04^d4), 8);
    a30 =   b0 ^((~b1)&  b2 );
    a11 =   b1 ^((~b2)&  b3 );
    a42 =   b2 ^((~b3)&  b4 );
    a23 =   b3 ^((~b4)&  b0 );
    a04 =   b4 ^((~b0)&  b1 );

    b1 = ROL64((a20^d0), 36);
    b2 = ROL64((a01^d1), 10);
    b3 = ROL64((a32^d2), 15);
    b4 = ROL64((a13^d3), 56);
    b0 = ROL64((a44^d4), 27);
    a20 =   b0 ^((~b1)&  b2 );
    a01 =   b1 ^((~b2)&  b3 );
    a32 =   b2 ^((~b3)&  b4 );
    a13 =   b3 ^((~b4)&  b0 );
    a44 =   b4 ^((~b0)&  b1 );

    b3 = ROL64((a10^d0), 41);
    b4 = ROL64((a41^d1), 2);
    b0 = ROL64((a22^d2), 62);
    b1 = ROL64((a03^d3), 55);
    b2 = ROL64((a34^d4), 39);
    a10 =   b0 ^((~b1)&  b2 );
    a41 =   b1 ^((~b2)&  b3 );
    a22 =   b2 ^((~b3)&  b4 );
    a03 =   b3 ^((~b4)&  b0 );
    a34 =   b4 ^((~b0)&  b1 );

    c0 = a00^a40^a30^a20^a10;
    c1 = a31^a21^a11^a01^a41;
    c2 = a12^a02^a42^a32^a22;
    c3 = a43^a33^a23^a13^a03;
    c4 = a24^a14^a04^a44^a34;
    d0 = c4^ROL64(c1, 1);
    d1 = c0^ROL64(c2, 1);
    d2 = c1^ROL64(c3, 1);
    d3 = c2^ROL64(c4, 1);
    d4 = c3^ROL64(c0, 1);

    b0 = (a00^d0);
    b1 = ROL64((a21^d1), 44);
    b2 = ROL64((a42^d2), 43);
    b3 = ROL64((a13^d3), 21);
    b4 = ROL64((a34^d4), 14);
    a00 =   b0 ^((~b1)&  b2 );
    a00 ^= RC[i+2];
    a21 =   b1 ^((~b2)&  b3 );
    a42 =   b2 ^((~b3)&  b4 );
    a13 =   b3 ^((~b4)&  b0 );
    a34 =   b4 ^((~b0)&  b1 );

    b2 = ROL64((a30^d0), 3);
    b3 = ROL64((a01^d1), 45);
    b4 = ROL64((a22^d2), 61);
    b0 = ROL64((a43^d3), 28);
    b1 = ROL64((a14^d4), 20);
    a30 =   b0 ^((~b1)&  b2 );
    a01 =   b1 ^((~b2)&  b3 );
    a22 =   b2 ^((~b3)&  b4 );
    a43 =   b3 ^((~b4)&  b0 );
    a14 =   b4 ^((~b0)&  b1 );

    b4 = ROL64((a10^d0), 18);
    b0 = ROL64((a31^d1), 1);
    b1 = ROL64((a02^d2), 6);
    b2 = ROL64((a23^d3), 25);
    b3 = ROL64((a44^d4), 8);
    a10 =   b0 ^((~b1)&  b2 );
    a31 =   b1 ^((~b2)&  b3 );
    a02 =   b2 ^((~b3)&  b4 );
    a23 =   b3 ^((~b4)&  b0 );
    a44 =   b4 ^((~b0)&  b1 );

    b1 = ROL64((a40^d0), 36);
    b2 = ROL64((a11^d1), 10);
    b3 = ROL64((a32^d2), 15);
    b4 = ROL64((a03^d3), 56);
    b0 = ROL64((a24^d4), 27);
    a40 =   b0 ^((~b1)&  b2 );
    a11 =   b1 ^((~b2)&  b3 );
    a32 =   b2 ^((~b3)&  b4 );
    a03 =   b3 ^((~b4)&  b0 );
    a24 =   b4 ^((~b0)&  b1 );

    b3 = ROL64((a20^d0), 41);
    b4 = ROL64((a41^d1), 2);
    b0 = ROL64((a12^d2), 62);
    b1 = ROL64((a33^d3), 55);
    b2 = ROL64((a04^d4), 39);
    a20 =   b0 ^((~b1)&  b2 );
    a41 =   b1 ^((~b2)&  b3 );
    a12 =   b2 ^((~b3)&  b4 );
    a33 =   b3 ^((~b4)&  b0 );
    a04 =   b4 ^((~b0)&  b1 );

    c0 = a00^a30^a10^a40^a20;
    c1 = a21^a01^a31^a11^a41;
    c2 = a42^a22^a02^a32^a12;
    c3 = a13^a43^a23^a03^a33;
    c4 = a34^a14^a44^a24^a04;
    d0 = c4^ROL64(c1, 1);
    d1 = c0^ROL64(c2, 1);
    d2 = c1^ROL64(c3, 1);
    d3 = c2^ROL64(c4, 1);
    d4 = c3^ROL64(c0, 1);

    b0 = (a00^d0);
    b1 = ROL64((a01^d1), 44);
    b2 = ROL64((a02^d2), 43);
    b3 = ROL64((a03^d3), 21);
    b4 = ROL64((a04^d4), 14);
    a00 =   b0 ^((~b1)&  b2 );
    a00 ^= RC[i+3];
    a01 =   b1 ^((~b2)&  b3 );
    a02 =   b2 ^((~b3)&  b4 );
    a03 =   b3 ^((~b4)&  b0 );
    a04 =   b4 ^((~b0)&  b1 );

    b2 = ROL64((a10^d0), 3);
    b3 = ROL64((a11^d1), 45);
    b4 = ROL64((a12^d2), 61);
    b0 = ROL64((a13^d3), 28);
    b1 = ROL64((a14^d4), 20);
    a10 =   b0 ^((~b1)&  b2 );
    a11 =   b1 ^((~b2)&  b3 );
    a12 =   b2 ^((~b3)&  b4 );
    a13 =   b3 ^((~b4)&  b0 );
    a14 =   b4 ^((~b0)&  b1 );

    b4 = ROL64((a20^d0), 18);
    b0 = ROL64((a21^d1), 1);
    b1 = ROL64((a22^d2), 6);
    b2 = ROL64((a23^d3), 25);
    b3 = ROL64((a24^d4), 8);
    a20 =   b0 ^((~b1)&  b2 );
    a21 =   b1 ^((~b2)&  b3 );
    a22 =   b2 ^((~b3)&  b4 );
    a23 =   b3 ^((~b4)&  b0 );
    a24 =   b4 ^((~b0)&  b1 );

    b1 = ROL64((a30^d0), 36);
    b2 = ROL64((a31^d1), 10);
    b3 = ROL64((a32^d2), 15);
    b4 = ROL64((a33^d3), 56);
    b0 = ROL64((a34^d4), 27);
    a30 =   b0 ^((~b1)&  b2 );
    a31 =   b1 ^((~b2)&  b3 );
    a32 =   b2 ^((~b3)&  b4 );
    a33 =   b3 ^((~b4)&  b0 );
    a34 =   b4 ^((~b0)&  b1 );

    b3 = ROL64((a40^d0), 41);
    b4 = ROL64((a41^d1), 2);
    b0 = ROL64((a42^d2), 62);
    b1 = ROL64((a43^d3), 55);
    b2 = ROL64((a44^d4), 39);
    a40 =   b0 ^((~b1)&  b2 );
    a41 =   b1 ^((~b2)&  b3 );
    a42 =   b2 ^((~b3)&  b4 );
    a43 =   b3 ^((~b4)&  b0 );
    a44 =   b4 ^((~b0)&  b1 );
  }
}

/*
** Initialize a new hash.  iSize determines the size of the hash
** in bits and should be one of 224, 256, 384, or 512.  Or iSize
** can be zero to use the default hash size of 256 bits.
*/
static void SHA3Init(SHA3Context *p, int iSize){
  memset(p, 0, sizeof(*p));
  if( iSize>=128 && iSize<=512 ){
    p->nRate = (1600 - ((iSize + 31)&~31)*2)/8;
  }else{
    p->nRate = (1600 - 2*256)/8;
  }
#if SHA3_BYTEORDER==1234
  /* Known to be little-endian at compile-time. No-op */
#elif SHA3_BYTEORDER==4321
  p->ixMask = 7;  /* Big-endian */
#else
  {
    static unsigned int one = 1;
    if( 1==*(unsigned char*)&one ){
      /* Little endian.  No byte swapping. */
      p->ixMask = 0;
    }else{
      /* Big endian.  Byte swap. */
      p->ixMask = 7;
    }
  }
#endif
}

/*
** Make consecutive calls to the SHA3Update function to add new content
** to the hash
*/
static void SHA3Update(
  SHA3Context *p,
  const unsigned char *aData,
  unsigned int nData
){
  unsigned int i = 0;
  if( aData==0 ) return;
#if SHA3_BYTEORDER==1234
  if( (p->nLoaded % 8)==0 && ((aData - (const unsigned char*)0)&7)==0 ){
    for(; i+7<nData; i+=8){
      p->u.s[p->nLoaded/8] ^= *(u64*)&aData[i];
      p->nLoaded += 8;
      if( p->nLoaded>=p->nRate ){
        KeccakF1600Step(p);
        p->nLoaded = 0;
      }
    }
  }
#endif
  for(; i<nData; i++){
#if SHA3_BYTEORDER==1234
    p->u.x[p->nLoaded] ^= aData[i];
#elif SHA3_BYTEORDER==4321
    p->u.x[p->nLoaded^0x07] ^= aData[i];
#else
    p->u.x[p->nLoaded^p->ixMask] ^= aData[i];
#endif
    p->nLoaded++;
    if( p->nLoaded==p->nRate ){
      KeccakF1600Step(p);
      p->nLoaded = 0;
    }
  }
}

/*
** After all content has been added, invoke SHA3Final() to compute
** the final hash.  The function returns a pointer to the binary
** hash value.
*/
static unsigned char *SHA3Final(SHA3Context *p){
  unsigned int i;
  if( p->nLoaded==p->nRate-1 ){
    const unsigned char c1 = 0x86;
    SHA3Update(p, &c1, 1);
  }else{
    const unsigned char c2 = 0x06;
    const unsigned char c3 = 0x80;
    SHA3Update(p, &c2, 1);
    p->nLoaded = p->nRate - 1;
    SHA3Update(p, &c3, 1);
  }
  for(i=0; i<p->nRate; i++){
    p->u.x[i+p->nRate] = p->u.x[i^p->ixMask];
  }
  return &p->u.x[p->nRate];
}
/* ************************************************************************** */
/* End of the hashing logic */

/* ************************************************************************** */
/* The original file was truncated at this point.
 * The following code is Copyright (C) 2022 by Kadena LLC.
 */

/* ************************************************************************** */
/* Implementation of SHA3 aggregate functions
 *
 * Return a BLOB which is the SIZE-bit SHA3 hash of X.
 * If X (or any subsequent argument) is a BLOB, it is hashed as is.
 * For all other types of input, the respective argument is converted
 * into a UTF-8 string and the string is hashed without the trailing 0x00
 * terminator.
 */
static void sha3StepFunc (int iSize, sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
  SHA3Context *cx;
  int eType = sqlite3_value_type(argv[0]);
  int nBytes;
  int i;

  cx = (SHA3Context*) sqlite3_aggregate_context(ctx, sizeof(SHA3Context));
  if (cx->nRate == 0) {
    SHA3Init(cx, iSize);
  }

  for (i = 0; i < argc; ++i) {
    eType = sqlite3_value_type(argv[i]);
    nBytes = sqlite3_value_bytes(argv[i]);

    if (eType == SQLITE_BLOB) {
      SHA3Update(cx, sqlite3_value_blob(argv[i]), nBytes);
    } else {
      SHA3Update(cx, sqlite3_value_text(argv[i]), nBytes);
    }
  }
}

static void sha3StepFunc_224 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3StepFunc(224, c, ac, av); }
static void sha3StepFunc_256 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3StepFunc(256, c, ac, av); }
static void sha3StepFunc_384 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3StepFunc(384, c, ac, av); }
static void sha3StepFunc_512 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3StepFunc(512, c, ac, av); }

static void sha3FinalFunc (int iSize, sqlite3_context *ctx)
{
  SHA3Context *cx;
  cx = (SHA3Context*) sqlite3_aggregate_context(ctx, sizeof(SHA3Context));
  if (cx->nRate == 0) {
    SHA3Init(cx, iSize);
  }
  sqlite3_result_blob(ctx, SHA3Final(cx), iSize/8, SQLITE_TRANSIENT);
}

static void sha3FinalFunc_224 (sqlite3_context *c) { sha3FinalFunc(224, c); }
static void sha3FinalFunc_256 (sqlite3_context *c) { sha3FinalFunc(256, c); }
static void sha3FinalFunc_384 (sqlite3_context *c) { sha3FinalFunc(384, c); }
static void sha3FinalFunc_512 (sqlite3_context *c) { sha3FinalFunc(512, c); }

/* ************************************************************************** */
/* Implementation of SHA3 scalar functions.
 *
 * Return a BLOB which is the SIZE-bit SHA3 hash of X.
 * If X (or any subsequent argument) is a BLOB, it is hashed as is.
 * For all other types of input, the respective argument is converted
 * into a UTF-8 string and the string is hashed without the trailing 0x00
 * terminator.
 */
static void sha3Func (int iSize, sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
  SHA3Context cx;
  int i;
  int eType;
  int nBytes;

  SHA3Init(&cx, iSize);

  for (i = 0; i < argc; ++i) {
    eType = sqlite3_value_type(argv[i]);
    nBytes = sqlite3_value_bytes(argv[i]);

    if (eType == SQLITE_BLOB) {
      SHA3Update(&cx, sqlite3_value_blob(argv[i]), nBytes);
    } else {
      SHA3Update(&cx, sqlite3_value_text(argv[i]), nBytes);
    }
  }

  sqlite3_result_blob(ctx, SHA3Final(&cx), iSize/8, SQLITE_TRANSIENT);
}

static void sha3Func_224 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3Func(224, c, ac, av); }
static void sha3Func_256 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3Func(256, c, ac, av); }
static void sha3Func_384 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3Func(384, c, ac, av); }
static void sha3Func_512 (sqlite3_context *c, int ac, sqlite3_value **av) { sha3Func(512, c, ac, av); }

/* ************************************************************************** */
/* Initialize all SHA3 functions
 */
int sqlite3_shathree_create_functions(sqlite3 *db)
{
  int rc = SQLITE_OK;
  const int rep = SQLITE_UTF8 | SQLITE_INNOCUOUS | SQLITE_DETERMINISTIC;

  /* Aggregate Functions */
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3a", -1, rep, 0, 0, sha3StepFunc_256, sha3FinalFunc_256);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3a_224", -1, rep, 0, 0, sha3StepFunc_224, sha3FinalFunc_224);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3a_256", -1, rep, 0, 0, sha3StepFunc_256, sha3FinalFunc_256);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3a_384", -1, rep, 0, 0, sha3StepFunc_384, sha3FinalFunc_384);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3a_512", -1, rep, 0, 0, sha3StepFunc_512, sha3FinalFunc_512);
  }

  /* Scalar Functions */
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3", -1, rep , 0, sha3Func_256, 0, 0);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3_224", -1, rep , 0, sha3Func_224, 0, 0);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3_256", -1, rep , 0, sha3Func_256, 0, 0);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3_384", -1, rep , 0, sha3Func_384, 0, 0);
  }
  if (rc == SQLITE_OK) {
    rc = sqlite3_create_function(db, "sha3_512", -1, rep , 0, sha3Func_512, 0, 0);
  }

  return rc;
}

/* ************************************************************************** */
/* SQLite Extension
 *
 * When compiled as shared library this supports dynamic loading of the
 * extension.
 */
int sqlite3_shathree_init(
  sqlite3 *db,
  char **pzErrMsg,
  const sqlite3_api_routines *pApi
){
  int rc = SQLITE_OK;
  SQLITE_EXTENSION_INIT2(pApi);
  sqlite3_shathree_create_functions(db);
  return rc;
}

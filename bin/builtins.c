/* Dylan M. | Ronit S. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

/* VECTORS */

struct Vector
{
  int length;
  int capacity;
  void *list;
};

struct Vector *Vector_new()
{
  struct Vector *l = (struct Vector *)malloc(sizeof(struct Vector));
  l->capacity = 10;
  l->length = 0;
  l->list = malloc(l->capacity * sizeof(void *));
  assert(l->list != NULL);

  return l;
}

void *Vector_get(struct Vector *v, int index)
{
  assert(index >= 0);
  assert(index < v->length);
  return *(void **)(v->list + (index * sizeof(void *)));
}

void Vector_free(struct Vector *v)
{
  for (int i = 0; i < v->length; i++)
  {
    void *addr = *(void **)(v->list + (i * sizeof(void *)));
    free(addr);
  }

  free(v->list);
  free(v);
}

void Vector_grow(struct Vector *v)
{
  v->capacity = v->capacity * 2;
  v->list = realloc(v->list, v->capacity * sizeof(void *));
  assert(v->list != NULL);
}

void Vector_shrink(struct Vector *v)
{
  v->capacity = v->capacity / 2;
  v->list = realloc(v->list, v->capacity * sizeof(void *));
  assert(v->list != NULL);
}

void Vector_push(struct Vector *v, void **new_value)
{
  if (v->length + 1 >= v->capacity)
  {
    Vector_grow(v);
  }

  void *addr = v->list + (v->length * sizeof(void *));
  addr = memcpy(addr, new_value, (size_t)sizeof(void *));
  v->length++;
}

void Vector_pop(struct Vector *v)
{
  assert(v->length > 0);

  free((void *)Vector_get(v, v->length - 1));

  v->length--;
  if (v->length < v->capacity / 2)
  {
    Vector_shrink(v);
  }

  return;
}

/* STRINGS */

void *Str_new(char *s)
{
  char *result = (char *)malloc(strlen(s) + 1);
  assert(result != NULL);
  strcpy(result, s);

  return (void *)result;
}

char *Str_concat(char *s1, char *s2)
{
  char *result = (char *)malloc(strlen(s1) + strlen(s2) + 1);
  assert(result != NULL);

  strcpy(result, s1);
  strcat(result, s2);

  return result;
}

void Str_push(char **s, char c)
{
  *s = (char *)realloc(*s, (strlen(*s) + 2) * sizeof(char));
  assert(s != NULL);
  strncat(*s, &c, 1);
  return;
}

int Str_compare(char *s1, char *s2)
{
  return strcmp(s1, s2);
}

char *Str_clone(char *s)
{
  char *duped = strdup(s);
  assert(duped != NULL);
  return duped;
}

/* RNG (ISO C99 spec) */
static unsigned long int next = 1;

int Rng_generate(int min, int max)
{
  next = next * 1103515245 + 12345;
  return (unsigned int)(((next / 65536) % (max - min + 1)) + min);
}

void Rng_init(int seed)
{
  if (seed < 0)
  {
    next = time(NULL);
  }
  else
  {
    next = seed;
  }
}
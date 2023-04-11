#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

  return l;
}

void *Vector_get(struct Vector *v, int index)
{
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
}

void Vector_grow(struct Vector *v)
{
  v->capacity = v->capacity * 2;
  v->list = realloc(v->list, v->capacity * sizeof(void *));
}

void Vector_shrink(struct Vector *v)
{
  v->capacity = v->capacity / 2;
  v->list = realloc(v->list, v->capacity * sizeof(void *));
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

void *Vector_pop(struct Vector *v)
{
  assert(v->length > 0);

  void *value = Vector_get(v, v->length - 1);
  v->length--;

  if (v->length < v->capacity / 2)
  {
    Vector_shrink(v);
  }

  return value;
}

/* STRINGS */

void *Str_new(char *s)
{
  char *result = (char *)malloc(strlen(s) + 1);
  strcpy(result, s);

  return (void *)result;
}

char *Str_concat(char *s1, char *s2)
{
  char *result = (char *)malloc(strlen(s1) + strlen(s2) + 1);

  strcpy(result, s1);
  strcat(result, s2);

  free(s1);
  free(s2);

  return result;
}

int Str_compare(char *s1, char *s2)
{
  return strcmp(s1, s2);
}

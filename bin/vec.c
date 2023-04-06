#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Vector {
  int length;
  int capacity;
  void *list;
};

struct Vector *Vector_alloc() {
  struct Vector *l = (struct Vector *)malloc(sizeof(struct Vector));
  l->capacity = 10;
  l->length = 0;
  l->list = malloc(l->capacity * sizeof(void *));
  return l;
}

void *Vector_get(struct Vector *v, int index) {
  // TODO: throw with err message
  assert(index < v->length);

  return *(void **)(v->list + (index * sizeof(void *)));
}

// TODO: do this in codegen OCAML
void Vector_free(struct Vector *v) {
  for (int i = 0; i < v->length; i++) {
    void *addr = *(void **)(v->list + (i * sizeof(void *)));
    free(addr);
  }

  free(v->list);
  free(v);
}

void Vector_grow(struct Vector *v) {
  v->capacity = v->capacity * 2;
  v->list = realloc(v->list, v->capacity * sizeof(void *));
}

void Vector_shrink(struct Vector *v) {
  v->capacity = v->capacity / 2;
  v->list = realloc(v->list, v->capacity * sizeof(void *));
}

void Vector_push(struct Vector *v, void **new_value) {
  if (v->length + 1 >= v->capacity) {
    Vector_grow(v);
  }

  void *addr = v->list + (v->length * sizeof(void *));
  printf("hi\n");
  addr = memcpy(addr, new_value, (size_t)sizeof(void *));
  printf("hi2\n");

  v->length++;
}

void *Vector_pop(struct Vector *v) {
  // TODO: throw err message
  assert(v->length > 0);

  void *value = Vector_get(v, v->length - 1);

  v->length--;

  if (v->length < v->capacity / 2) {
    Vector_shrink(v);
  }

  return value;
}

// int test_vec_create() {
//   struct Vector *l = Vector_alloc();

//   for (int i = 0; i < 11; i++) {
//     int *val = malloc(sizeof(int));
//     *val = i;
//     Vector_push(l, (void *)val);
//   }

//   printf("vector capacity: %d\n", l->capacity);

//   for (int i = 0; i < 6; i++) {
//     void *out = Vector_pop(l);
//     free(out);
//   }

//   printf("vector capacity: %d\n", l->capacity);

//   for (int i = 0; i < l->length; i++) {
//     printf("value at index %d %d\n", i, *(int *)Vector_get(l, i));
//   }

//   Vector_free(l);

//   return 0;
// }

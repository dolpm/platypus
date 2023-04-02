#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

struct List {
	int length;
	int capacity;
	void* list;
};

struct List* Vector_alloc () {
	struct List *l = (struct List *)malloc(sizeof(struct List));
	l->capacity = 10;
	l->length = 0;
	l->list = malloc(l->capacity * sizeof(void*));	
	
	return l;
}

void* Vector_get (struct List *v, int index) {
	// TODO: throw with err message
	assert(index < v->length);
	
	return *(void**)(v->list + (index * sizeof(void*)));
}

void Vector_free (struct List *v) {
	for (int i = 0; i < v->length; i++) {
		void* addr = *(void**)(v->list + (i * sizeof(void*)));
		free(addr);
	}

	free(v->list);
	free(v);
}

void Vector_grow (struct List* v) {
	v->capacity = v->capacity + 10;
	void* new_chunk = malloc((v->capacity) * sizeof(void*));
	void* copied_list = memcpy(new_chunk, v->list, (size_t) v->length);
	free(v->list);
	v->list = copied_list;
}

void Vector_shrink (struct List *v) {
	v->capacity = v->capacity / 2;
	v->list = realloc(v->list, (size_t) v->capacity);
}

void Vector_push (struct List *v, void *new_value) {
	if (v->length + 1 >= v->capacity) {
		Vector_grow(v);
	}

	void* addr = v->list + (v->length * sizeof(void*)); 
	addr = memcpy(addr, &new_value, (size_t) sizeof(void*));
	v->length ++;
}

void* Vector_pop (struct List *v) {
	// TODO: throw err message
	assert (v->length > 0);

	void* value = v->list + ((v->length +1) * sizeof(void*));
	v->length --;

	if (v->length < v->capacity /2) {
		Vector_shrink(v);
	}
	
	return value;
}

int main () {
	struct List* l = Vector_alloc();
	int *val1 = malloc(sizeof(int));
	*val1 = 26;
	
	int *val2 = malloc(sizeof(int));
	*val2 = 1738;
	

	Vector_push(l, (void*) val1);
	Vector_push(l, (void*) val2);

	int* x = (int *)Vector_get(l, 0);
		
	printf("pooped value: %d\n", *x);
	
	Vector_free(l);

	return 0;
}

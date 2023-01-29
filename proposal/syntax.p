/* example imports */
take "./bar.p";
/* we could also do partial ones like take foo from "./bar.p" */

/* types */
thing /* struct */
int
float
bool
tuple[x, y, z] /* unit is just an empty tuple () */
str /* sized string (stack) */
string /* re-sizeable string (ptr to heap) */
array[x] /* sized array (stack) (all types) */
collection[x] /* re-sizeable array (ptr to heap) (all types) */
heap[x] /* smart-pointer for heap-allocs (all types) */

thing MyThing = [
  /* vis denotes something visible from outside of an attribute implementation. */
  /* by default, all children are not visible to prevent accidental visibility */
  vis name: string,
  priv_name: string,
];

/* this gives us a crude form of polymorphism / shared behavior between */
/* our things. it's the equivalent to an interface in c++ and a trait in rust */
attr Helpers {
  get_name: [self: &self] |> ();
  set_name: [self: ~&self, new_name: string] |> ();
}

/* this will implement these functions for all MyThing's */
/* all inner functions here must take a reference to a mything (mutable or not) */
impl Helpers on MyThing {
  get_name: [self: &MyThing] |> () {
    printnl("The name of this thing is: {}!", self.name);
    |> ();
  },
  set_name: [self: ~&MyThing, new_name: string] |> () {
    self.name <| new_name;
    |> ();
  },
}

pipe create_thing |> [] 
                  |> () 
{
  /* create a fluid (mutable) instance of MyThing, denoted by the `~` */
  ~MyThing thing <| MyThing {
    name: "thing1";
  };

  /* we could also probably do something to make the explicit */
  /* definition of the &self argument optional. also, the */
  /* "() _ <|" here is optional but is just showing how */
  /* variable assignment works in this case */
  () _ <| (get_name <| thing) <| [&thing];

  (set_name <| thing) <| [~&thing, "thing2"];

  (set_name <| thing) <| [&thing];

  /* should print out "thing2\nthing1\n" */
  |> ();
}

pipe fib |> [n: int] 
         |> int 
{
	if n < 0 {
		panic("err");
	}

	if n == 0 {
		|> 0;
	} else if n == 1 and n == 2 {
		|> 1;
	}
	
	int minus_one <| fib <| [n - 1];
	int minus_two <| fib <| [n - 2];

	|> minus_one + minus_two;

  /* idea: we could also have some helpers for all types to coerce a */
  /* un-piped function expression to the executed values so that we can do something */
  /* like int(fib <| [n - 1]) + int(fib <| [n - 2]) instead of these 3 lines */
  /* either that or just wrapping a un-piped function will be equivalent () in */
  /* most other languages (i.e., (fib <| [n-1]) === fib(n-1)) */
}

/* values allocated on the heap live for the lifetime of their ownership. */
/* they just represent a reference managed by the runtime (i.e., smart-ptr) */
/* references to heap values are handled like normal references, but think of */
/* them as references-to-references under the hood */

/* this function just allocates heap space for the value 1024 and returns it */
/* which passes ownership to the caller of this function */

/* this is kinda a stupid usecase as you can just return 1024 but it gets */
/* the point accross */

/* Heap functions are obviously generated for all types, as you should */
/* be able to throw any type on the heap (including user-generated ones) */
pipe test_heap_alloc |> [] 
                     |> heap[int]
{
  |> ((alloc <| Heap) <| [1024]) 
}
/* idea: we could also use dot notation (i.e., (Heap.alloc <| [1024])) */
/* i'm not sure what makes more sense, but it feels weird to use it this way */
/* without parenthesis wrapping `alloc <| Heap` to show 1024 is being piped */
/* into Heap.alloc and not just Heap */

/* also, by default Collections (vectors) and string types should live on */
/* the stack BUT point to the heap since their sizes aren't known at compile */
/* time. we could have something like a string slice type which is of */
/* constant size (i.e., it can be stored entirely on the stack). */
/* similarly, we can have an array type whose size is known at compile time. */
/* this may add an extra layer of complexity, but could be done. */

pipe main |> [] |> () {
  /* can probably make <| [] implicit? */
  /* on contrary, what if we did something like the above to create a */
  /* function pointer, and then to call it we had to do something like */
  /* _ () <| create_thing <| []  (i.e., pipe the function into some binding) */
  (create_thing <| []);

  /* example fib call */
  int n <| fib <| [10];
}
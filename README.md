# sum
## Copyright
Public domain, 2023

github.com/SirJonthe

## About
`sum` is a minimalist checksum/hashing library for C++11.

## Design
`sum` provides a number of different checksum/hashing algorithms, each contained inside their own class. Each of these classes use the exact same streamlined public interface which makes it easy to replace one algorithm for another. `sum` also completely avoids allocating memory dynamically, making it suitable for embedded purposes.

## Building
No special adjustments need to be made to build `sum` except enabling C++11 compatibility or above. Simply include the relevant headers in your code and make sure the headers and source files are available in your compiler search paths. Using `g++` as an example, building is no harder than:

```
g++ -std=c++11 code.cpp sum/sum.cpp
```

...where `code.cpp` is an example source file containing the user-defined unit tests as well as the entry point for the program.

## Examples
### Generating a checksum/hash
A checksum can be generated via the following method:
```
#include "sum/sum.h"

int main()
{
	cc0::sum::md5 h;
	h.ingest("A string");
	auto checksum = h.digest();
	return 0;
}
```
Note that the hashing function used in the example above, `cc0::sum::md5`, can be replaced by any of the other hashing functions provided in the library and still work as expected.

All hashing algorithms work on void pointers and char pointers.

The hashing algorithm instance carries the state of the hashing algorithm. This makes it easy to append more data to the checksum, making it easy to stream data.

```
#include "sum/sum.h"

int main()
{
	cc0::sum::md5 h;
	h.ingest("Some data");
	auto checksum = h.digest();
	h.ingest("Some more data");
	checksum = h.digest();
	return 0;
}
```
The code example above is equivalent to hashing all the data at once, like so:
```
#include "sum/sum.h"

int main()
{
	cc0::sum::md5 h;
	h.ingest("Some dataSome more data");
	auto checksum = h.digest();
	return 0;
}
```
The benefit of being able to split up data to be hashed can be seen when hashing should be done for contents of large files, where only a small portion of the file needs to be loaded at any one given time rather than the whole file.

### Concise hashing
Generating a checksum can be done incredibly concise if there is no need to pass the hashing algorithm class around:
```
#include "sum/sum.h"

using namespace cc0::sum;

int main()
{
	md5::sum sum = md5("1")("2")("3");
	return 0;
}
```
This works due to three reasons; Each hashing algorithm has constructors that mirror their `ingest` function, overloads `()` to mirror their `ingest` function (and also returns a reference to itself so that `()` can be chained together), and implicitly converts to the checksum type.

### Converting a checksum to string
Checksums can not be converted into strings directly as `sum` avoids memory allocations on the heap as well as STL. The contents of a checksum can, however, be printed to an existing string using `sprint_hex` and `sprint_bin`:
```
#include "sum/sum.h"

using namespace cc0::sum;

int main()
{
	md5::sum sum = md5("1")("2")("3");
	static constexpr uint64_t hex_str_chars = sizeof(md5::sum) * 2 + 1;
	char hex_str[hex_str_chars];
	hex_str[hex_str_chars] = 0;

	sum.sprint_hex(hex_str);

	return 0;
}
```
The `sprint_*` functions return a pointer to the point on the string it wrote up until. This pointer can be used to know at what point it is safe to add more string data in the string without overwriting the human-readable checksum.

### Comparing checksums
Checksums implement comparison operators making comparing checksums of the same type trivial:
```
#include "sum/sum.h"

using namespace cc0::sum;

int main()
{
	md5::sum a = md5("1")("2")("3");
	md5::sum b = md5("2")("3")("4");
	if (a < b) {
		return 1;
	}
	return 0;
}
```
Note however that comparing checksums this way principally makes little sense, but can still be useful when used as keys in search structures which assume the programmer has overloaded comparison operators.

## Dangers
Some hashing algorithms in this library are considered cryptographical. There are no guarantees that the implementations in this library function properly, meaning this library should not be used where security is required.

## About rights
It is important to note that the CC0 license only covers this code as such, and not the algorithms contained therein per se.

## Future work
* More hashing functions?
* Add some compile-time hashing functions?

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <caml/mlvalues.h>

typedef int64_t (*jit_fn)(void); // function ptr type

CAMLprim value run_code(value bytes){
    // convert to 
    unsigned char *code = (unsigned char *)Bytes_val(bytes);
    // allocate executable memory
    size_t code_size = caml_string_length(bytes);

    void* mem = mmap(NULL, code_size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (mem == MAP_FAILED) {
        perror("mmap failed");
        return Val_int(1);
    }

    // copy code into executable memory
    memcpy(mem, code, code_size);

    // change memory protection
    if (mprotect(mem, code_size, PROT_READ | PROT_EXEC) == -1) {
        perror("mprotect failed");
        munmap(mem, code_size);
        return Val_int(1);
    }

    // cast memory into func ptr (we treat ptr as pointing to the function description)
    // call function
    int64_t res = ((jit_fn)mem)();
    
    // free memory
    munmap(mem, code_size);
    return Val_long(res);
}
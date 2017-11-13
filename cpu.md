What does a cpu need to do:
* it needs to decode data from code Memory
* given the decoding it needs to read from external memories
* given the read it needs to permute internal state
* give internal state it needs to write out to external memory 
* it needs to specify where in code memory data should come from next

This is a standard pipeline topology, but can be reduced into a simple cpu as well. 

Goal is to abstract away the concept of what a cpu core is, it's instruction set
its internal cpu state, etc. Seperate those from the core effects of the cpu such as 
reads and writes to main memory as well as the execution paradigm (pipelined,etc)
If we can abstract the basic features of a ALU then we can seperate it out.

```
class ALU state where 
    read :: instr -> state -> readinst
    process :: instr -> state -> readval -> state
    write :: instr -> state -> writeinstr 
    move :: instr -> state -> codeloc
    nop :: instr
```

In this case readinst would be a data type that tells the underlying system what value to read from
which memory. so maybe we inject it such as `data MemRead = IO (Unsigned 8) | Main (Unsigned 16)`
or something of that ilk. Likewise, readval would be the data result of the read (eg `Unsigned 8`). 
Finally `writeinstr` would be similar to readinstr except it would also contain data. In this scenario,
we require some additional functionality for pipelining such as `readinstr -> writeinstr -> Bool` which tells
us if the read and write will conflict. 

Caveat: there are many different types of memory models which cpus interact with.
For example, some CPUs have both main data memory as well as a seperate class of 
IO interfaces or ports.

Question: how do we support variable types of memory?

I think it is safe to assume code memory is read-only, or that there will be no
races when it comes to reading from code memory. However, it is not safe to assume
this for other types. 

Other considerations are that we want the CPU to compose with the memory, not control it. 
This means we would like the system to be reduced at somepoint to an entity which signals memory instead of 
contains it.  




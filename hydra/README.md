# Communication between MC (meta-contract) and instrumented code

## Calldata

The instrumented contract can only be called by itself and the MC. Since the MC
acts as a call forwarder, it needs a way to forward some of the attributes of a
call to an instrumented contract: The first word of CALLDATA will be the address
of the CALLER of the MC. The second word of CALLDATA will be the CALLVALUE the
MC received. When the MC calls the instrumented contract it will always call
with a CALLVALUE of 0. 

For calls to self, the instrumentation will follow the same calling convention:
The first word of CALLDATA is the address of the MC, and  the second word of
CALLDATA is the CALLVALUE, while the actual CALLVALUE will be 0.

## Return data

Whenever the instrumented code RETURNs to the MC, the first word of the return
data will contain the size of the return data in bytes (not including the first word).
Example: If the non-instrumented contract RETURNs 2 bytes, [0x20, 0xf3], the
         instrumented contract RETURNs 32+2 bytes, [0x00, 0x00, 0x00, 0x00,
         0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
         0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
         0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x20, 0xf3]

Note that we can use STOP to return empty return data, because the EVM will set
any "overflowing" return data to zero.

## Callbacks into the Metacontract

Whenever the instrumented contract wants to interact with the outside world,
the interaction has to be routed through the MC so that it can be recorded.
Right now, only the following interactions are supported:

- LOGing events
- CALLing other contracts
- checking the BALANCE of accounts

The instrumented contract will call the MC so that it can perform the
the interaction on its behalf. Any such call has CALLVALUE = 0.
The last word of the CALLDATA indicates the type of interaction:

```
    last word | meaning
    ----------+--------
            0 | LOG0
            1 | LOG1
            2 | LOG2
            3 | LOG3
            4 | LOG4
            5 | CALL
            6 | BALANCE
```

TODO(lorenzb): More documentation
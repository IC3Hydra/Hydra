# Instrumentation

There are (conceptually) 5 different contracts:
- Outer metacontract
- Inner metacontract
- Callback metacontract
- First head
- Other heads

The three metacontracts are actually the same contract:
- When the MC receives a call from the first head, it acts as the callback metacontract.
- When the MC receives a call from itself, it acts as the inner metacontract.
- Otherwise, the MC acts as the outer metacontract.

## Outer metacontract

Receives calls from the outside world.

Only calls inner metacontract.

No special input or output format.

Responsible for reentrancy guard, bounty payouts, and escape hatch.

## Inner metacontract

Only accepts calls from metacontract.

Only calls heads.

Input format: `[caller, callvalue] ++ calldata`

Output format:
- Success: `SUCCESS([1] ++ returndata) `
- Failure: `SUCCESS([0] ++ returndata)`
- Disagreement: `FAIL([0xd15a9])`
- OOG: `FAIL([])`
- Instrumentation Error: `FAIL([code])`

## Callback metacontract

Only accepts calls from first head.

Input format:
- for `LOG0`: `logdata ++ [0]`
- for `LOG1`: `logdata ++ [topic1, 1]`
- for `LOG2`: `logdata ++ [topic1, topic2, 2]`
- for `LOG3`: `logdata ++ [topic1, topic2, topic3, 3]`
- for `LOG4`: `logdata ++ [topic1, topic2, topic3, topic4, 4]`
- for `CALL`: `calldata ++ [to, value, 5]`

Output format:
- for `LOG*`: `SUCCESS([])`
- for `CALL`: `SUCCESS([success] ++ returndata)`

## First head

Only accepts calls from inner metacontract and itself.

Only calls callback metacontract and itself.

Input format: `[caller, callvalue, calldata_size] ++ calldata`

Output format:
- Success: `SUCCESS([1, trace_size] ++ trace ++ returndata)`
- Failure: `FAIL([1, trace_size] ++ trace ++ returndata)`
- OOG: `FAIL([])`
- Instrumentation Error: `FAIL([code])`

### Trace

The trace is a concatenation of records.
We compress inputs by hashing them, but store full outputs.

Record format:
- `LOG0`: `[0, keccak256(logdata)]`
- `LOG1`: `[1, keccak256(logdata ++ [topic1])]`
- `LOG2`: `[2, keccak256(logdata ++ [topic1, topic2])]`
- `LOG3`: `[3, keccak256(logdata ++ [topic1, topic2, topic3])]`
- `LOG4`: `[4, keccak256(logdata ++ [topic1, topic2, topic3, topic4])]`
- `CALL`: `[5, keccak256(calldata ++ [to, value]), success, output_size] ++ output`
- `BALANCE`: `[6, address, balance(address)]`

Due to the unclear semantics of advancing the trace inside a FAILing callframe, we forbid this behaviour for now.
TODO(lorenzb): Actually implement the corresponding asserts

## Other head

Only accepts calls from inner metacontract and itself.

Only calls itself.

Input format: `[caller, callvalue, calldata_size] ++ calldata ++ [trace_size] ++ trace`

Output format:
- Success: `SUCCESS([1, trace_read] ++ returndata)` 
- Failure: `FAIL([1, trace_read] ++ returndata)`
- Disagreement: `FAIL([0xd15a9])`
- OOG: `FAIL([])`
- Instrumentation Error: `FAIL([code])`

# Dealing with inner call complexities

If an OOG occurrs in an inner call, it is propagated all the way to the MC!

If an inner call fails, it must not return data and it must not advance the trace


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
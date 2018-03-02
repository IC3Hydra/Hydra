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
- for `LOG0`: `[0] ++ logdata`
- for `LOG1`: `[1, topic1] ++ logdata`
- for `LOG2`: `[2, topic1, topic2] ++ logdata`
- for `LOG3`: `[3, topic1, topic2, topic3] ++ logdata`
- for `LOG4`: `[4, topic1, topic2, topic3, topic4] ++ logdata`
- for `CALL`: `[5, to, value] ++ calldata`

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
- `LOG0`: `[0, keccak256([keccak256(logdata)])]`
- `LOG1`: `[1, keccak256([keccak256(logdata), topic1])]`
- `LOG2`: `[2, keccak256([keccak256(logdata), topic1, topic2])]`
- `LOG3`: `[3, keccak256([keccak256(logdata), topic1, topic2, topic3])]`
- `LOG4`: `[4, keccak256([keccak256(logdata), topic1, topic2, topic3, topic4])]`
- `CALL`: `[5, keccak256([keccak256(calldata), to, value]), success, output_size] ++ output`
- `BALANCE`: `[6, address, balance(address)]`
- `EXTCODESIZE`: `[7, address, extcodesize(address)]`

Due to the unclear semantics of advancing the trace inside a FAILing callframe, we forbid this behaviour for now.
TODO(lorenzb): Actually implement the corresponding asserts (done for HeadN, not for HeadOne)


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

## Requirements

The `evm` tool is part of [go-ethereum](https://github.com/ethereum/go-ethereum).

## Compilation

To assemble/compile `foo.easm`, simply run

    evm compile foo.easm

Since `evm compile` sometimes behaves strangely (e.g. for `PUSH*` opcodes),
it's best to verify the output by running

    evm disasm <(evm compile foo.easm)

## Debugging

If you need to debug a contract, the following command is a good start:

    evm --codefile <(evm compile foo.easm) --debug --dump run 2>&1 | less



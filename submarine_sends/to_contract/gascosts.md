Protocol: Repeatedly run

    receiver.createCloneAtCommitAddress(2,2, {from: eth.accounts[0], gas: 3e6})

on local myrtle testnet (see genesis-myrtle.json) until there is a clone at the commit address.

I did three runs, each of which took 4 transactions. The gascosts were

    2907873 + 2939648 + 2923881 + 743381 = 9514783
    2905331 + 2938377 + 2917526 + 1504053 = 10265288
    2909144 + 2937106 + 2920068 + 1028633 = 9794951

So receiving funds costs ~10e6 gas. At a gasprice of 20e9 wei, this is 0.2 eth.

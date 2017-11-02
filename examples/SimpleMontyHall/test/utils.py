from ethereum import utils as u
from ethereum.tools.tester import TransactionFailed
from hashlib import sha256
from enum import IntEnum, Enum


def to_wei(ether):
    return int(ether * 10**18)

MIN_DEPOSIT = to_wei(0.1)
MAX_VAL = 2**120


def get_balance(chain, account):
    return chain.head_state.get_balance(account[0])


def commit(winning_door, witness):
    m = sha256()
    m.update(u.zpad(u.encode_int(winning_door), 1))
    m.update(witness)
    return m.digest()


def encode_doors(opened_doors):
    res = 0
    for i in range(32):
        res |= (opened_doors[i] * 2**i)

    for i in range(32):
        assert ((res & (2**i)) != 0) == opened_doors[i]

    return res


class GameStages(IntEnum):
    INIT = 0
    ROUND1 = 1
    OPEN = 2
    ROUND2 = 3
    ENDGAME = 4

    PAYOUT = 0
    ESCAPE = 1
    REFUND_INACTIVE = 2
    REFUND_AFTER_HATCH = 3


class ErrorMsgs(Enum):
    INIT_MONTY_FAILED = "InitMonty Failed"
    INIT_MONTY_RETURNED_VAL = "InitMonty returned a value"
    PLAY_MONTY_ROUND_1_FAILED = "PlayMontyRound1 Failed"
    PLAY_MONTY_ROUND_1_RETURNED_VAL = "PlayMontyRound1 returned a value"
    OPEN_DOOR_FAILED = "OpenDoor Failed"
    PLAY_MONTY_ROUND_2_FAILED = "PlayMontyRound2 Failed"
    END_GAME_FAILED = "EndGame Failed"
    PAYOUT_FAILED = "Payout Failed"
    ESCAPE_FAILED = "EscapeHatch Failed"
    REFUND_INACTIVE_FAILED = "RefundInactive Failed"
    REFUND_AFTER_HATCH_FAILED = "RefundAfterEscapeHatch Failed"


def play_game(chain, contract, gi, rounds=range(GameStages.ENDGAME + 1), value=0):
    senders = [v[1] for v in gi["senders"]]
    ids = gi["game_ids"]

    all_rounds = [
        (lambda: contract.InitMonty(gi["k"], gi["n"], gi["commit"],
                                    value=gi["reward"] + gi["deposit"],
                                    sender=senders[0]),
         ErrorMsgs.INIT_MONTY_FAILED, None),

        (lambda: contract.PlayMontyRound1(gi["guess1"], ids[1],
                                          value=gi["bet"], sender=senders[1]),
         ErrorMsgs.PLAY_MONTY_ROUND_1_FAILED, None),

        (lambda: contract.OpenDoors(encode_doors(gi["opened_doors"]), ids[2],
                                    value=value, sender=senders[2]),
         ErrorMsgs.OPEN_DOOR_FAILED, True),

        (lambda: contract.PlayMontyRound2(gi["guess2"], ids[3],
                                          value=value, sender=senders[3]),
         ErrorMsgs.PLAY_MONTY_ROUND_2_FAILED, True),

        (lambda: contract.EndGame(gi["winning_door"], gi["witness"], ids[4],
                                  value=value, sender=senders[4]),
         ErrorMsgs.END_GAME_FAILED, True)
    ]

    events = []
    chain.head_state.log_listeners.append(
        lambda x: events.append(contract.translator.listen(x))
    )

    for i in rounds:
        func, err_msg_fail, expected_ret_val = all_rounds[i]

        try:
            ret = func()
        except TransactionFailed:
            raise RuntimeError(err_msg_fail)

        if ret != expected_ret_val:
            raise RuntimeError("Wrong return value: Expected {} Got {}".format(
            expected_ret_val, ret))

    return True


def special_func(contract, game_id, func_id, sender, value=0, **kwargs):
    sender = sender[1]

    all_funcs = [
        (lambda: contract.Payout(kwargs.get("bool", False), game_id,
                                 value=value, sender=sender),
         ErrorMsgs.PAYOUT_FAILED, True),

        (lambda: contract.EscapeHatch(value=value, sender=sender),
         ErrorMsgs.ESCAPE_FAILED, True),

        (lambda: contract.RefundInactive(game_id, value=value, sender=sender),
         ErrorMsgs.REFUND_INACTIVE_FAILED, True),

        (lambda: contract.RefundAfterEscapeHatch(game_id,
                                                 value=value, sender=sender),
         ErrorMsgs.REFUND_AFTER_HATCH_FAILED, True),
    ]

    func, err_msg_fail, expected_ret_val = all_funcs[func_id]

    try:
        ret = func()
    except TransactionFailed:
        raise RuntimeError(err_msg_fail)

    if ret != expected_ret_val:
        raise RuntimeError("Wrong return value: Expected {} Got {}".format(
            expected_ret_val, ret))

    return True

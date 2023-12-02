test = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""


def parse_pair(text: str) -> tuple[str, int]:
    n, color = text.split(" ")
    return int(n), color


def parse_round(text: str) -> list[tuple[str, int]]:
    return list(map(
        parse_pair,
        text.split(", ")
    ))


def parse_line(text: str):
    head, games = text.split(": ")
    game_id = int(head.removeprefix("Game "))

    rounds = list(map(
        parse_round,
        games.split("; ")
    ))

    return game_id, rounds


def parse_games(text: str) -> list[tuple[int, list[list[tuple[str, int]]]]]:
    return list(map(
        parse_line,
        text.split("\n")
    ))


def satisfies_round(contraints, round) -> bool:
    return all(
        n <= contraints[color]
        for n, color in round
    )


def satisfies(contraints, game) -> bool:
    return all(
        satisfies_round(constraints, round)
        for round in game[1]
    )


constraints = {
    "red": 12,
    "green": 13,
    "blue": 14
}


def solve1(text: str):
    games = parse_games(text)

    return sum(game[0] for game in games if satisfies(constraints, game))


def minimum(game) -> int:
    def m(c):
        return max((n for round in game[1] for n, color in round if color == c), default=1)

    return m("green") * m("red") * m("blue")


def solve2(text: str):
    games = parse_games(text)

    return sum(minimum(game) for game in games)


def main1():
    with open("input.txt", "r") as fp:
        text = fp.read().strip()

    print(solve1(text))


def main2():
    with open("input.txt", "r") as fp:
        text = fp.read().strip()
    print(solve2(text))

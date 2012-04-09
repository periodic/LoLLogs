var cursor = db.Game.find();

while (cursor.hasNext()) {
    var g = cursor.next();
    g.gameStats.summoners = g.gameStats.summoners.map(function (s) {
        return s.toLowerCase();
    });
    g.gameStats.blueTeam = g.gameStats.blueTeam.map(function (s) {
        return s.toLowerCase();
    });
    g.gameStats.purpleTeam = g.gameStats.purpleTeam.map(function (s) {
        return s.toLowerCase();
    });

    for (var summoner in g.gameStats.playerStats) {
        var lcSummoner = summoner.toLowerCase();
        if (summoner != lcSummoner) {
            var stats = g.gameStats.playerStats[summoner];
            g.gameStats.playerStats[lcSummoner] = stats
            delete g.gameStats.playerStats[summoner];
        }
    }

    db.Game.save(g);
};

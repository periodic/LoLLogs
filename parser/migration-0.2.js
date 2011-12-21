
function keys(obj) {
    var keys = [];
    for (var k in obj) {
        if (obj.hasOwnProperty(k)) keys.push(k);

    };
    return keys
}

function updateGame(g)  {
    var pTeam = g.gameStats.teamPlayerParticipantStats;
    var oTeam = g.gameStats.otherTeamPlayerParticipantStats;
    var summoners = keys(pTeam).concat(keys(oTeam));

    var blueTeam, purpleTeam;
    if (pTeam[summoners[0]].teamId == 100) {
        blueTeam = keys(pTeam);
        purpleTeam = keys(oTeam)
        } else {blueTeam = keys(oTeam);
        purpleTeam = keys(pTeam)
    };

    var stats = {};
    for (var k in pTeam) {
        stats[k] = pTeam[k];
    };
    for (var k in oTeam) {
        stats[k] = oTeam[k]
    };

    var champions = [];
    for (var k in pTeam) {
        champions.push(pTeam[k].skinName);
    };
    for (var k in oTeam) {
        champions.push(oTeam[k].skinName);
    };

    g.gameStats.summoners = summoners;
    g.gameStats.blueTeam = blueTeam;
    g.gameStats.purpleTeam = purpleTeam;
    g.gameStats.playerStats = stats;
    g.gameStats.champions = champions;
    delete(g.gameStats.teamPlayerParticipantStats);
    delete(g.gameStats.otherTeamPlayerParticipantStats);
}

db.Game.find().forEach(function (g) {
    updateGame(g);
    db.GameCopy.save(g);
});


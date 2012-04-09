/*
var spells = [
    {
        ident: NumberLong(1),
        name: 'Cleanse',
        imageName: 'Cleanse'
    },
    {
        ident: NumberLong(2),
        name: 'Clairvoyance',
        imageName: 'Clairvoyance'
    },
    {
        ident: NumberLong(5),
        name: 'Exhaust',
        imageName: 'Exhaust'
    },
    {
        ident: NumberLong(4),
        name: 'Flash',
        imageName: 'Flash'
    },
    { 
        ident: NumberLong(5),
        name: "Revive",
        imageName: 'Revive'
    },
    { 
        ident: NumberLong(6),
        name: "Ghost",
        imageName: 'Ghost'
    },
    {
        ident: NumberLong(7), 
        name: 'Heal',
        imageName: 'Heal'
    },
    { 
        ident: NumberLong(8),
        name: "Promote",
        imageName: 'Promote'
    },
    { 
        ident: NumberLong(9),
        name: "Forify",
        imageName: 'Fortify'
    },
    { 
        ident: NumberLong(10),
        name: "Clarity",
        imageName: 'Clarity'
    },
    { 
        ident: NumberLong(11),
        name: "Smite",
        imageName: 'Smite'
    },
    { 
        ident: NumberLong(12),
        name: "Teleport",
        imageName: 'Teleport'
    },
    { 
        ident: NumberLong(13),
        name: "Rally",
        imageName: 'Rally'
    },
    {
        ident: NumberLong(14),
        name: 'Ignite',
        imageName: 'Ignite'
    },
    { 
        ident: NumberLong(15),
        name: "Observer",
        imageName: 'Observer'
    },
    { 
        ident: NumberLong(16),
        name: "Stifle",
        imageName: 'Stifle'
    },
];
*/
var spells = [
    'Cleanse',      // 1
    'Clairvoyance', // 2
    'Exhaust',      // 3
    'Flash',        // 4
    "Rally",        // 5
    "Ghost",        // 6
    'Heal',         // 7
    "Promote",      // 8
    "Forify",       // 9
    "Revive",       // 10
    "Smite",        // 11
    "Teleport",     // 12
    "Clarity",      // 13
    "Ignite",       // 14
    "Surge",        // 15
    "Garison"       // 16
];

db.Spell.drop();

var id = 1;
spells.forEach(function (s) {
    db.Spell.insert({
        ident: NumberLong(id),
        name: s,
        imageName: s
    });
    id++;
});

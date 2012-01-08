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
        ident: NumberLong(3),
        name: "Revive",
        imageName: 'Revive'
    },
    {
        ident: NumberLong(4),
        name: 'Flash',
        imageName: 'Flash'
    },
    {
        ident: NumberLong(5),
        name: 'Exhaust',
        imageName: 'Exhaust'
    },
    { 
        ident: NumberLong(6),
        name: "Promote",
        imageName: 'Promote'
    },
    {
        ident: NumberLong(7), 
        name: 'Fortify',
        imageName: 'Fortify'
    },
    { 
        ident: NumberLong(8),
        name: "Ghost",
        imageName: 'Ghost'
    },
    { 
        ident: NumberLong(9),
        name: "Heal",
        imageName: 'Heal'
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
]

db.Spell.drop();

spells.forEach(function (s) {
    db.Spell.insert(s);
});

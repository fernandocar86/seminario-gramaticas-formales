g = [ ([],[('sel','V'),('cat','C')]),
        ([],[('sel','V'),('pos','wh'),('cat','C')]),
        (['the'],[('sel','N'),('cat','D')]), 
        (['which'],[('sel','N'),('cat','D'),('neg','wh')]), 
        (['king'],[('cat','N')]),
        (['queen'],[('cat','N')]),
        (['wine'],[('cat','N')]),
        (['beer'],[('cat','N')]),
        (['drinks'],[('sel','D'),('sel','D'),('cat','V')]),
        (['prefers'],[('sel','D'),('sel','D'),('cat','V')]),
        (['knows'],[('sel','C'),('sel','D'),('cat','V')]),
        (['says'],[('sel','C'),('sel','D'),('cat','V')])
        ]
# Oraciones aceptadas:
# the queen drinks the wine
# the king drinks the wine
# the queen prefers the wine
# the queen says the king prefers the wine
# etc.

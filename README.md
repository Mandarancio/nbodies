# nbodies

Siple N Body simulation in Haskell (rendered using SDL2)

**Have fun ;)**

![Demo](docs/demo.gif)

## Setup and run

```bash
stack build
stack exec nbodies config/cfg.yml
```

## Configuration
The configuration is a Yaml file and has this structure:

```yaml
--- # Configuration
qtree: true                # display Quad Tree
barnesHut: true            # enable/disable Barnes Hut simulation
g: 1                       # universal gravitational constant
dT: 0.2                    # time interval of the simulation
scale: 1                   # simulation scale (0: auto)
bodies:                    # list of bodies
  - pos: {x:   0., y: 0.}  # position
    mass: 100.             # mass
    mom: {x:   0., y: 0.}  # momentum (speed)
```

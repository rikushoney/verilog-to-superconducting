import circuitbuilder as cb

def lut2():
    c = cb.Component()
    c.ports([
        cb.clock(name="clk"),
        cb.input(name="pci"),
        cb.input(name="pdi"),
        cb.output(name="pco"),
        cb.output(name="pdo"),
        cb.input(name="in_c", width=2),
        # TODO: needs special attention
        cb.input(name="in", width=2),
        cb.output(name="out")
    ])
    return c

def psrff():
    c = cb.Component()
    c.ports([
        # TODO: need special attention
        cb.input(name="set"),
        cb.clock(name="clk"),
        cb.output(name="out"),
        cb.input(name="en")
    ])
    return c

def ble2_sync():
    c = cb.Component()
    c.ports([
        cb.input(name="in", width=2),
        cb.output(name="out"),
        cb.clock(name="pclk"),
        cb.clock(name="gclk")
    ])
    lut = lut2().make_ref()
    ff = psrff().make_ref()
    c.ports(lut.ports())
    c.ports(ff.ports())
    c.children([lut, ff])
    return c

if __name__ == "__main__":
    c = ble2_sync()
    print(c)

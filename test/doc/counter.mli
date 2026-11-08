type props = {step: int; label: string}

type out = Changed of int

include Kilim.COMPONENT with type props := props and type out := out

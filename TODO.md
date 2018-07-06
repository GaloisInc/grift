# grift TODO

Summary of pending tasks and things to think about.

## Decoding and encoding

Right now, the decoding and encoding are very nice, but purely
operational/functional. I am considering a more denotational style in correspondence
with my semantics representation. The decoding itself is actually a part of the
semantics. It would be incredibly cool to capture the entire fetch/decode/execute
process in a purely denotational way. Right now I have a denotational representation
for execution of each instruction, and I could easily imagine doing the same for
instruction fetch. But decoding is not currently captured in this way, and I imagine
it could be useful to do so.

## Compressed instructions

I can pretty easily add support for the C extension operationally, albeit in a
somewhat more ad-hoc way than the very nice encoding/decoding style I'm using for the
base ISA.

However, I'm considering holding off on implementing this until I determine whether
it would be useful to capture the decoding in a more declarative style. If I were to
do that, I would want to skip the operational style-encoding for C entirely, and just
do a denotational approach.

## Privileged architecture

Still need to do some more research here.

## Extensions

SoftFloat -- talk to Flemming when I get to this

## Exporting to other formats

What to do first? Should be fun.

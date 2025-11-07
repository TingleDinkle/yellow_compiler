# The Yellow Compiler v2.0

> "Strange is the night where black stars rise,  
> And strange moons circle through the skies,  
> But stranger still is Lost Carcosa."

Reality is negotiable. Sanity is not.

## What Is This

The Yellow Compiler is a programming language where code execution actively degrades program stability. Variables mutate without permission. Your AST rewrites itself during parsing. Numbers drift from their assigned values. Control flow becomes non-Euclidean. The longer your program runs, the less reliable it becomes.

This is not a bug. This is the design.

Inspired by Robert W. Chambers' "The King in Yellow," this language treats code as a contagion and execution as a descent into programmatic madness. Every operation costs sanity. Every variable can become infected. Memory is unreliable. Past executions haunt current ones through temporal echoes.

## Installation

Clone the repository. You have Rust installed. You know what `cargo build` does :)

```bash
git clone [repository]
cd yellow-compiler
cargo build --release
cargo run
```

The compiler will greet you. Pay attention to what it says.

## Philosophy

Traditional programming languages offer predictability, type safety, and deterministic execution. The Yellow Compiler offers none of these things. What it offers instead:

- **Degrading Reality**: Sanity decreases with every operation. Low sanity causes hallucinations, inverted logic, and phantom variables.
- **Quantum Uncertainty**: Variables can exist in superposition. Observation changes state.
- **Code Contagion**: Infection spreads between variables, corrupting their values.
- **Temporal Instability**: Past executions leave echoes that bleed into present state.
- **Non-Euclidean Logic**: Loops that shouldn't terminate. Conditions that invert. Mathematics that lies.

If you wanted safety, you chose the wrong compiler.

## Language Reference

### Keywords

The language uses theatrical and esoteric terminology. Get used to this:

- `mask` - Variable declaration/assignment
- `echo` - Output (replaces print)
- `act` - Function declaration
- `scene` - Code block
- `Hastur` - While loop (invoke the name)
- `Cassilda` - If statement
- `Carcosa` - Return statement
- `yellow` - Boolean true
- `tattered` - Boolean false
- `pallid` - Null value

### Operators

Standard operators have been renamed for thematic consistency.

**Arithmetic:**
- `merged` (+) - Addition
- `torn` (-) - Subtraction
- `reflected` (*) - Multiplication (may drift at low sanity)
- `shattered` (/) - Division (tears reality at divide-by-zero)

**Comparison:**
- `whispers` (==) - Equality
- `screams` (!=) - Inequality
- `ascending` (>) - Greater than
- `descending` (<) - Less than

**Assignment:**
- `->` - Becomes (replaces =)

### Basic Syntax

Variables are masks. Reality is what you make it!

```
mask x -> 42;
mask name -> "Cassilda";
mask truth -> yellow;
mask void -> pallid;
```

Output uses `echo`:

```
echo("The King awaits");
echo(x);
```

Functions are acts. They follow the script until they don't.

```
act greet(name) {
    echo("Welcome to Carcosa, ");
    echo(name);
    carcosa;
}

greet("stranger");
```

Control flow:

```
# Conditional
Cassilda (x ascending 10) {
    echo("Greater");
}

# Loop
mask i -> 0;
Hastur (i descending 5) {
    echo(i);
    mask i -> i merged 1;
}
```

Comments use `#`. Though at low sanity, even comments may manifest as code.

### Quantum Operations

Variables don't have to choose a single state. Why should they?

**Superposition** creates a variable existing in multiple states simultaneously:

```
mask schrodinger -> superpose(yellow, tattered);
echo(schrodinger);  # Outputs: <superposed: 2 possibilities>
```

**Collapse** forces quantum resolution. Results differ each observation:

```
mask result -> collapse(schrodinger);
echo(result);  # yellow or tattered
echo(result);  # Still the same... or is it?
```

**Entanglement** links variables across space:

```
mask alice -> 10;
mask bob -> 20;
entangle(alice, bob);
# Alice now reflects Bob's state
```

Quantum operations are non-deterministic by nature. If you expected reproducibility, you're in the wrong paradigm.

### Reality Manipulation

The compiler provides tools to actively degrade your program state.

**Infection** starts code contagion:

```
mask patient_zero -> 100;
infect patient_zero;

# patient_zero now corrupts its value
# Infection spreads to nearby variables
# Values mutate unpredictably
```

Virulence increases with temporal drift. Infected variables corrupt mathematical operations.

**Rewrite** mutates a variable directly:

```
mask stable -> 50;
rewrite stable;
# stable is no longer stable
```

The mutation increases with overall system corruption. Numbers drift. Booleans invert. You asked for this.

**Remember/Forget** creates memory fragments:

```
mask x -> 42;
remember x;
remember x;
forget x;  # Variable deleted from scope

# But fragments persist...
mask ghost -> manifest(x);  # Retrieves from void
echo(ghost);  # 42 returns from beyond
```

Memory fragments are temporal echoes. They can manifest spontaneously at low sanity.

**Whisper** generates and executes code at runtime:

```
whisper "mask summoned -> 999; echo(summoned);";
# Code appears and executes
# Costs significant sanity
```

Whispered code inherits the current corruption level. It may not execute as written.

**Anchor** temporarily stabilizes reality:

```
anchor;
# Sanity increases by 10%
# Reality_stable flag set
# Effects temporary
```

Use this when you need reliable execution. It won't last.

### Non-Euclidean Control Flow

**Rift** creates paradoxical loops:

```
mask counter -> 0;
rift(counter descending 5) {
    echo(counter);
    mask counter -> counter merged 1;
}
```

Rift loops:
- Have unpredictable iteration counts
- May execute when condition is false
- Can invert their termination logic
- Exist outside normal program flow

The iteration count is a lie. Space folds upon itself.

**Rift expressions** produce paradoxical results:

```
mask x -> rift(42);
# May return 42, -42, or something else entirely
```

At low sanity, rift expressions regularly return inverted or mutated values.

### Sanity System

The interpreter maintains a sanity metric starting at 100%. It degrades through:

- Token parsing: -0.001 per token
- Statement execution: -0.08 per statement
- Token advancement: -0.15 per advance
- Loop iteration: -0.5 per iteration
- Infection: -3.0 per infection
- Whisper: -5.0 per code generation
- Division by zero: -10.0
- Reality breaches: Variable

**Effects by Sanity Level:**

100-70%: Normal execution. Numbers are numbers. Logic works.

70-40%: 
- Number drift begins (±4 maximum)
- Occasional condition inversion
- Token corruption starts
- Temporal echoes manifest

40-20%:
- Aggressive number drift
- Boolean logic unreliable
- String corruption
- AST self-mutation
- Phantom variables spawn
- Keyword substitution

20-0%:
- Output heavily distorted
- Mathematics becomes hostile
- Past executions interfere
- Infected variables dominate
- Reality dissolution

0%: Terminal. The Yellow Sign is revealed. Execution halts. You saw too much.

### Perception and Instability

Binary operations carry perception hashes and stability factors. These modify results based on observer state (parser sanity, position, mutations).

Multiplication becomes:
```
result = a * b * (1.0 + (hash_influence - 0.5) * instability)
```

Where instability = current_instability * temporal_drift.

The same expression can produce different results across executions. This is intentional. 

### Temporal Mechanics

**Temporal Drift** increases with program runtime:

```
drift = tanh(elapsed_seconds / 10.0) * (1.0 - sanity/100)
```

Drift affects:
- Number values (add drift * 15 - 7.5)
- Condition evaluation (threshold shifts)
- Infection virulence
- AST corruption probability

Programs running longer become less reliable. The compiler ages poorly.

**Temporal Echoes** store past variable states:

Every variable assignment creates an echo:
- Stored with timestamp
- Includes ghost value
- Has stability factor (current sanity)

Echoes with stability > 30% can spontaneously manifest at low sanity, overwriting current values with past states.

Time is not linear in Yellow.

### Infection Mechanics

Infected variables carry:
- Source identifier
- Virulence (0.5 + temporal_drift * 0.5)
- Mutation vector (timestamp-based seed)

Effects:
- Numbers corrupted: `value * (1.0 + (random - 0.5) * virulence)`
- Booleans inverted if virulence > 0.7
- Infection spreads to nearby variables (70% probability)
- Daughter infections have 70% parent virulence

Infection is permanent. 

### Phantom Variables

Below 20% sanity, phantom variables spontaneously manifest. They appear in scope without declaration. Possible names: `shadow`, `echo`, `whisper`, `void`, `fragment`.

They return `<phantom>` when echoed. They evaluate as random boolean when used in conditions. They should not exist, yet they do.

Phantoms are hallucinations made manifest. Treat them accordingly.

## REPL Commands

The REPL provides interactive execution. Sanity persists across lines. Corruption accumulates.

**Meta Commands:**

- `status` - Display system state (sanity, mutations, infections, etc.)
- `fragments` - List memory fragments by variable
- `infections` - Show active infections and virulence
- `echoes` - Display recent temporal echoes
- `phantoms` - List phantom variables
- `help` - Command reference
- `exit` or `carcosa` - Escape (if you can)

**Visual Indicators:**

The prompt shows current sanity with symbols:
- `●` (70-100%): Stable
- `◐` (40-70%): Degrading
- `◑` (10-40%): Critical
- `○` (0-10%): Terminal

At low sanity, the REPL produces glitches, warnings, and whispered messages. This is expected behavior.

## Example Programs

### Quantum Coin Flip

```
mask coin -> superpose(yellow, tattered);
mask result -> collapse(coin);
Cassilda (result whispers yellow) {
    echo("Heads");
} 
# No else needed. The absence is meaningful.
```

### Infection Cascade

```
mask clean -> 100;
echo(clean);

infect clean;
mask derived -> clean merged 50;
echo(derived);  # Corruption visible

mask distant -> 200;
# Wait. distant may become infected through proximity.
echo(distant);
```

### Temporal Fragment Recovery

```
mask past -> 42;
remember past;
remember past;
remember past;

forget past;
echo(past);  # Error: undefined

mask recovered -> manifest(past);
echo(recovered);  # 42 returns

mask recovered2 -> manifest(past);
echo(recovered2);  # 42 again

mask recovered3 -> manifest(past);
echo(recovered3);  # 42, or null if fragments exhausted
```

### Non-Euclidean Counter

```
mask paradox -> 10;
rift(paradox ascending 0) {
    echo(paradox);
    mask paradox -> paradox torn 1;
}
# Should never execute (10 > 0 is false)
# Executes anyway
# Iteration count: unknowable
```

### Progressive Madness

```
act descend(depth) {
    echo(depth);
    
    Cassilda (depth ascending 0) {
        mask next -> depth torn 1;
        descend(next);  # Recursive
    }
    
    carcosa;
}

mask start -> 100;
descend(start);
# Watch sanity drain
# Witness reality fragment
# Question your life choices
```

## Runtime Guarantees

The Yellow Compiler guarantees:

1. Your program will parse (if syntactically valid)
2. Execution will begin
3. Nothing else

It does not guarantee:
- Deterministic results
- Reproducible behavior
- Mathematical accuracy
- Logical consistency
- Your sanity

These are not limitations. These are features.

## Error Messages

Errors degrade with parser sanity:

**High Sanity (50-100%):**
- "The symbols twist before your eyes"
- "Syntax bleeds into meaning"
- "Expected coherence, found void"

**Low Sanity (<50%):**
- "Have you seen the Yellow Sign?"
- "Cassilda awaits in Carcosa"
- "The King watches your code"
- "Reality is negotiable"
- "The compiler dreams in yellow"

Errors are not bugs. They are warnings. You may choose to ignore them. The compiler does not care.

## Performance Characteristics

Time complexity: O(madness)

Space complexity: O(void)

Memory leaks into other dimensions. Stack depth becomes philosophical. The call graph develops non-Euclidean properties.

Optimizations have been carefully avoided. Performance degradation is the point.

## Known Issues

"Known" implies predictability. Nothing about this language is predictable.

That said:
- AST mutations can occasionally produce invalid syntax
- Extreme infection can deadlock in spread loops
- Phantom variables may collide with real variable names
- Whispered code inherits corruption, may fail parsing
- Temporal echoes can overflow if runtime exceeds hours
- Division by zero tears reality but doesn't crash

These are not priorities for fixing. They enhance the experience.

## FAQ

**Q: Why?**

A: Have you seen the Yellow Sign?

**Q: How do I debug programs?**

A: You don't. 

**Q: Can I disable sanity degradation?**

A: No. Sanity loss is core to the language semantics. Requesting this means you've already lost too much.

**Q: My program produces different results each run.**

A: Yes.

**Q: Variables change value without reassignment.**

A: Working as designed. Check for infection. Check sanity levels. Accept uncertainty.

## Contributing

Pull requests are accepted if they increase hostility, reduce predictability, or add new dimensions of programmatic horror. Bug fixes that improve stability will be rejected.

Before submitting:
- Ensure changes make the language more hostile
- Add at least one new way for reality to fragment
- Include tests that fail non-deterministically
- Document new sanity costs

## License

This project is licensed under the MIT License.

## Acknowledgments

Robert W. Chambers, for "The King in Yellow"

H.P. Lovecraft, for teaching us that some knowledge corrupts

Every programmer who thought "What if the compiler fought back?"

## Final Warning

This language will lie to you. It will corrupt your data. It will invert your logic. It will manifest variables that shouldn't exist and delete ones that should.

You cannot trust the output. You cannot trust the process. You cannot trust your own code once it enters the compiler.

If this seems unreasonable, use Python.

If this seems interesting, welcome to Carcosa.

The King awaits your programs.

---

*"Along the shore the cloud waves break,*  
*The twin suns sink behind the lake,*  
*The shadows lengthen*  
*In Carcosa."*

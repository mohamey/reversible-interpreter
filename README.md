# Reversible Interpreter
Reversible Interpreter for Topics in Functional Programming. The following features have been implemented:
* Turn project into a stack project
* Provide a monadic interpreter for the language that allows you to step through the program
* Add an inspect command to examine the contents of a variable
* Augment interpreter so that it records the history of each assignment
* Add a step backwards command that allows the user to move backwards through statements

## Usage
To build the project:
```
stack build
```

To run the interpreter with the program `testProg`:
```
stack exec reversible-interpreter-exe testProg
```

### Commands
| Command   | Action                            |
| --------- | --------------------------------- |
| step      | Execute the next function         |
| inspect a | Inspect the variable history of a |
| stepbw    | Step to the previous instruction  |
| exit      | Exit the interpreter              |

## Program Structure
Programs that can be parsed by the interpreter have to take the format of Statements as specified in `src/Interpreter.hs`. This means that `If` and `While` statements do not have enclosing brackets, and all parts of a single statement must remain on the same line. Statements should appear on their own lines.

```
Assign "a" (Add (Const (I 10)) (Const (I 20)))

If (Eq (Const (B True)) (Const (B False))) (stmt1) (stmt2)

While (Lt (Const (I 10)) (Const (I 20))) (stmt)

Seq (stmt1) (stmt2)

Try (stmt1) (stmt2)

Pass
```

## Task 2 - Monadic Interpreter with Step Function
This task has been fully completed and should work with any valid program written in the Statement language. To step through the program, just type "step" when prompted to input a command by the interpreter. The interpreter also displays what the next command is.

## Task 3 - Extend the Interpreter to inspect Variables
This task has been completed, to inspect a variable simply input "inspect var" where var is the name of the variable you want to inspect. This will print out all the values var has ever had, with the most recent values appearing at the top.

## Task 4 - Augment the Interpreter so that it records the history of each variable
This task has been completed. Keys in the Environment now map to lists which store the current value of the variable at the head and previous values in the tail (most recent first)

## Task 5 - Add a step backwards function
This task has been completed, by keeping three lists when the interpreter is running:
* A list of environments for previous statements
* A list of previous commands. The head of this list is the last command executed, and corresponds with the head of the environment list
* A list of commands to be executed

Stepping backwards is done by calling "stepbw". To step backwards, the interpreter deletes all keys currently in the environment and writes the head of the old environment list to the environment. This is to undo any variable modifications the previous statement might have made.

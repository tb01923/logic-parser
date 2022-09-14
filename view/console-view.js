const readline = require('readline');

const INPUT_COMMAND = "INPUT_COMMAND"
const NEXT_COMMAND = "NEXT_COMMAND"
const PRINT_COMMAND = "PRINT_COMMAND"

const optionsOptions = {
    INPUT_COMMAND: [],
    NEXT_COMMAND: ["input", "print"],
    PRINT_COMMAND: ["prior menu", "ast", "implicit statement", "statement", "debruinj"]
}

const prompt = {
    INPUT_COMMAND: "statement",
    PRINT_COMMAND: "print",
    NEXT_COMMAND: "ready"
}


class View {
    constructor() {
        this.controller = null
        this.command = NEXT_COMMAND
        this.rl = readline.createInterface(process.stdin, process.stdout)
    }

    setController = controller => {
        this.controller = controller
    }

    setPrompt = () => {
        const myPrompt = `${prompt[this.command]}(${optionsOptions[this.command].join("|")})>`
        this.rl.setPrompt(myPrompt)
    }

    output = line => {
        console.log(line)
    }

    setInputState = (command) => {
        this.command = command
        this.setPrompt()
    }

    promtUser = function() {
        const isInputCommand = line => (this.command === NEXT_COMMAND && line === "input")
        const isStatementCommand = line => (this.command === INPUT_COMMAND)
        const isPrintMenuCommand = line => (this.command === NEXT_COMMAND && line === "print")
        const isPrintAstCommand = line => (this.command === PRINT_COMMAND && line === "ast")
        const isPrintStatementCommand = line => (this.command === PRINT_COMMAND && line === "statement")
        const isPrintImplicitCommand = line => (this.command === PRINT_COMMAND && line === "implicit statement")
        const isPrintDeBruinjCommand = line  => (this.command === PRINT_COMMAND && line === "debruinj")

        const isExit = line => (line === "quit")

        this.setPrompt()
        this.rl.on('line', line => {
            if( isExit(line) ) {
                this.controller.exit()
            }
            else if ( isStatementCommand(line) ) {
                this.controller.parseInput(line)
                this.setInputState(NEXT_COMMAND)
            }
            else if ( isInputCommand(line) ) {
                this.setInputState(INPUT_COMMAND)
            }
            else if ( isPrintMenuCommand(line) ) {
                this.setInputState(PRINT_COMMAND)
            }
            else if ( isPrintAstCommand(line) ) {
                this.controller.displayAst()
            }
            else if ( isPrintStatementCommand(line) ) {
                this.controller.displayStatement()
            }
            else if ( isPrintImplicitCommand(line) ) {
                this.controller.displayStatementImplicit()
            }
            else if ( isPrintDeBruinjCommand(line) ) {
                this.controller.displayStatementImplicitDeBruinj()
            }
            this.rl.prompt()
        })
    }

    static from = () => {
        return new View()
    }
}

module.exports = View
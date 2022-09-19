const readline = require('readline');
const printVisitors = require("./printVisitors");

const INPUT_COMMAND = "INPUT_COMMAND"
const NEXT_COMMAND = "NEXT_COMMAND"
const PRINT_COMMAND = "PRINT_COMMAND"

const commandOptions = {
    INPUT_COMMAND: [],
    NEXT_COMMAND: ["input", "print", "recommend"],
    PRINT_COMMAND: ["prior menu", "ast", "implicit statement", "statement", "debruinj"]
}

const prompt = {
    INPUT_COMMAND: "statement",
    PRINT_COMMAND: "print",
    NEXT_COMMAND: "ready"
}

const getDebRuinjString = expression => {
    const debruinj = expression.accept(printVisitors.implicitDebruinj)
    return debruinj
}

const getStatementString = expression => {
    const statement = expression.accept(printVisitors.explicitStatement)
    return statement
}

const getImplicitStatementString = expression => {
    const statement = expression.accept(printVisitors.implicitStatement)
    return statement
}

const getAstString = expression => {
    const astString = expression.accept(printVisitors.ast)
    return astString
}


class View {
    constructor() {
        this.controller = undefined
        this.viewModel = undefined
        this.command = NEXT_COMMAND
        this.rl = readline.createInterface(process.stdin, process.stdout)
    }

    setViewModel = (viewModel) => {
        this.viewModel = viewModel
    }

    setController = (controller, viewModel) => {
        this.controller = controller
        this.viewModel = viewModel
    }

    isNextCommand = line => (this.command === NEXT_COMMAND)
    isInputCommand = line => (this.command === NEXT_COMMAND && line === "input")
    isPrintMenuCommand = line => (this.command === NEXT_COMMAND && line === "print")
    isRecommendCommand = line => (this.command === NEXT_COMMAND && line === "recommend")
    isApplyCommand = line => (this.command === NEXT_COMMAND && line.split(" ")[0] === "apply")
    isStatementCommand = line => (this.command === INPUT_COMMAND)
    isPriorMenuCommand = line => (this.command === PRINT_COMMAND && line === "prior")
    isPrintAstCommand = line => (this.command === PRINT_COMMAND && line === "ast")
    isPrintStatementCommand = line => (this.command === PRINT_COMMAND && line === "statement")
    isPrintImplicitCommand = line => (this.command === PRINT_COMMAND && line === "implicit statement")
    isPrintDeBruinjCommand = line  => (this.command === PRINT_COMMAND && line === "debruinj")
    isExit = line => (line === "quit")

    setPrompt = () => {
        const options = (commandOptions[this.command]).slice(0)
        if ( this.isNextCommand() && this.viewModel.matches.length > 0) {
            options.push("apply")
        }

        const myPrompt = `${prompt[this.command]}(${options.join("|")})>`
        this.rl.setPrompt(myPrompt)
    }

    output = line => {
        console.log(line)
    }

    setViewCommandState = (command) => {
        this.command = command
        this.setPrompt()
    }

    printRecommendations = () => {
        this.output(`option\trationale\t\t\texpression`)
        let count = 1 ;

        for (const matchedLaw of this.viewModel.matches) {
            this.output(`------\t-------------------------\t----------------------`)
            const matchedSegment = getStatementString(matchedLaw.expression)
            this.output(`\tmatched sub-expression id\t${matchedLaw.expression.id}`)
            this.output(`\tmatched sub-expression\t\t${matchedSegment}`)

            //const law = getStatementString(matchedLaw.applicableLaw)
            const law = matchedLaw.law
            this.output(`[${count}.]\t${matchedLaw.name}\t\t${law}`)

            const lawInALpha = getStatementString(matchedLaw.applicableLawInPreferredAlphabet)
            this.output(`\tapplied to sub-expression\t${lawInALpha}`)

            count = count + 1
        }
        this.output(`------\t-------------------------\t----------------------`)
    }

    applyRecommendation(recommendationIdx) {

        // identify match
        const match = this.viewModel.matches[recommendationIdx]

        // make replacement
        this.controller.replace(match.expression, match.applicableLawInPreferredAlphabet)

        // output new command
        const str = getStatementString(this.viewModel.originalExpression)
        this.output("new expression:" + str)
    }

    promtUser = function() {

        try {
            this.setPrompt()
            this.rl.on('line', line => {
                if (this.isExit(line)) {
                    this.controller.exit()
                } else if (this.isInputCommand(line)) {
                    this.setViewCommandState(INPUT_COMMAND)
                } else if (this.isStatementCommand(line)) {
                    this.controller.parseInput(line)
                    this.setViewCommandState(NEXT_COMMAND)
                } else if (this.isRecommendCommand(line)) {
                    this.controller.recommend()
                    this.printRecommendations()
                    this.setViewCommandState(NEXT_COMMAND)
                } else if (this.isApplyCommand(line)) {
                    const matchIndex = (line.split(" ")[1]) - 1
                    this.applyRecommendation(matchIndex)
                    this.setViewCommandState(NEXT_COMMAND)
                }

                /*
                    PRINT LOOP: TODO: refactor print loo
                 */
                else if (this.isPrintMenuCommand(line)) {
                    this.setViewCommandState(PRINT_COMMAND)
                } else if (this.isPriorMenuCommand(line)) {
                    this.setViewCommandState(NEXT_COMMAND)
                } else if (this.isPrintAstCommand(line)) {
                    const str = getAstString(this.viewModel.originalExpression)
                    this.output(str)
                } else if (this.isPrintStatementCommand(line)) {
                    const str = getStatementString(this.viewModel.originalExpression)
                    this.output(str)
                } else if (this.isPrintImplicitCommand(line)) {
                    const str = getImplicitStatementString(this.viewModel.originalExpression)
                    this.output(str)
                } else if (this.isPrintDeBruinjCommand(line)) {
                    const str = getDebRuinjString(this.viewModel.originalExpression)
                    this.output(str)
                }
                this.rl.prompt()
            })
        }
        catch(e) {
            console.log(e)
        }
    }

    static from = () => {
        return new View()
    }
}

module.exports = View
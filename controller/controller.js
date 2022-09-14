const { parse: parse1_0 } = require("../parser/1_0/parser")
const { parse: parse2_0 } = require("../parser/2_0/parser")
const parse = parse2_0

const printVisitors = require("./commands/printVisitors")

class Controller {
    constructor (view) {
        this.expression = null
        this.view = view;
        this.view.setController(this)
    }

    start = () => this.view.promtUser()

    exit = () => {
        process.exit(0)
    }

    displayAst = () => {
        const astString = this.expression.accept(printVisitors.ast)
        this.view.output(astString)
    }

    displayStatement = ()  => {
        console.log(this.expression.accept(printVisitors.explicitStatement))
    }

    displayStatementImplicit = ()  => {
        console.log(this.expression.accept(printVisitors.implicitStatement))
    }

    displayStatementImplicitDeBruinj = ()  => {
        console.log(this.expression.accept(printVisitors.implicitDebruinj))
    }

    parseInput = input => {
        this.expression = parse(input)
    }

    static from = (view) => {
        return new Controller(view)
    }
}

module.exports = Controller
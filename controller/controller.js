const { identifyLaws, restateInNewAlphabet } = require("./commands/propositional-laws")
const ViewModel = require("./view-model")
// const { parse: parse1_0 } = require("../parser/1_0/parser")
const { parse: parse2_0 } = require("../parser/2_0/parser")
const parse = parse2_0

class Controller {
    constructor (view) {
        this.expression = null
        this.viewModel = ViewModel.from()

        this.view = view;
        this.view.setController(this, this.viewModel)
    }

    start(){
        this.view.promtUser()
    }

    exit() {
        process.exit(0)
    }

    recommend () {
        const matches = identifyLaws(this.expression)
        const restatedMatches = restateInNewAlphabet(matches, this.expression.knownDebruinjIndexes)
        this.viewModel.matches = restatedMatches
        this.view.setViewModel(this.viewModel)
    }

    parseInput(input){
        this.expression = parse(input)
        this.expression.setDeBruinjIndex()
        this.viewModel.originalExpression = this.expression
    }

    replace(expressionToReplace, replacementExpression) {
        let isReplaced = false
        // in simple expressions you may replace the entire expression
        if (this.expression.id === expressionToReplace.id) {
            isReplaced = true
            this.expression = replacementExpression.clone()
        }
        // in complex expressions you may replace a portion of the expressions
        else {
            isReplaced = this.expression.replace(expressionToReplace.id, replacementExpression)
        }

        if (isReplaced) {
            // if we replace any bit, we should reindex the debruinj
            this.expression.setDeBruinjIndex()
            // set the viewModel
            this.viewModel.originalExpression = this.expression
            this.viewModel.matches = []
            this.view.setViewModel(this.viewModel)
        }
    }

    static from = (view) => {
        return new Controller(view)
    }
}

module.exports = Controller
class Vm {

    constructor () {
        this.originalExpression = undefined
        this.matches = []
    }

    static from = () => new Vm()
}

module.exports = Vm
class RecursiveVisitor {
    constructor (visitorDictionary) {
        this.VisitorDictionary = visitorDictionary
    }
    visit (node)  {
        const nodeClass = node.constructor.name
        const visitImpl = this.VisitorDictionary[nodeClass]
        const retval = visitImpl(this, node)
        return retval
    }
}

module.exports = Object.freeze({
    RecursiveVisitor
})
const peg = require('pegjs')
    , fs = require('fs')

const readUtf8File = fileName => fs.readFileSync(fileName, 'UTF-8')
const toParser = pegGrammar => peg.generate(pegGrammar)


const getParser = () => {
    const pegGrammar = readUtf8File('./parser/grammar.peg')
    return toParser(pegGrammar)
}

module.exports = Object.freeze({
    getParser
})
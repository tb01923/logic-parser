const peg = require('pegjs')
    , fs = require('fs')

const readUtf8File = fileName => fs.readFileSync(fileName, 'UTF-8')
const toParser = pegGrammar => peg.generate(pegGrammar)


const getParser = () => {
    const pegGrammar = readUtf8File('./parser/1_0/grammar.peg')
    return toParser(pegGrammar)
}

const parser = getParser()

const parse = line => parser.parse(line)


module.exports = Object.freeze({
    parse
})
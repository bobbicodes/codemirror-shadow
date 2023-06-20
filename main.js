import './style.css'
import { EditorView, basicSetup } from 'codemirror'
import { EditorState } from '@codemirror/state'
import { parser, props } from "@nextjournal/lezer-clojure"
import { styleTags, tags } from "@lezer/highlight"
import { Tree } from '@lezer/common'
import { indentNodeProp, foldNodeProp, foldInside, LRLanguage, LanguageSupport, syntaxTree } from "@codemirror/language"

const { coll } = props

export const clojureLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [styleTags({
      NS: tags.keyword,
      DefLike: tags.keyword,
      "Operator/Symbol": tags.keyword,
      "VarName/Symbol": tags.definition(tags.variableName),
      // Symbol: tags.keyword,
      // "'": tags.keyword, // quote
      Boolean: tags.atom,
      "DocString/...": tags.emphasis,
      "Discard!": tags.comment,
      Number: tags.number,
      StringContent: tags.string,
      "\"\\\"\"": tags.string, // need to pass something, that returns " when being parsed as JSON
      Keyword: tags.atom,
      Nil: tags.null,
      LineComment: tags.lineComment,
      RegExp: tags.regexp
    }),

    indentNodeProp.add((nodeType) => {
      return (context) => {
        let { pos, unit, node, state, baseIndent, textAfter } = context
        if (nodeType.prop(coll)) {
          // same behaviour as in clojure-mode: args after operator are always 2-units indented
          let parentBase = context.column(node.firstChild.to) // column at the right of parent opening-(
          if ("List" == nodeType.name && ["NS", "DefLike", "Operator"].includes(node.firstChild.nextSibling.type.name)) {
            return parentBase + 1
          } else {
            return parentBase
          }
        } else {
          return 0
        }
      }
    }),

    foldNodeProp.add({ ["Vector Map List"]: foldInside })]
  }),

  languageData: { commentTokens: { line: ";;" } }
})

export function clojure() {
  return new LanguageSupport(clojureLanguage)
}

let state = EditorState.create({
  doc: `(map inc (range 8))`,
  extensions: [basicSetup, clojure()]
})

new EditorView({
  state: state,
  parent: document.querySelector('#app')
}).focus()

function tree(state, pos, dir) {
  switch (arguments["length"]) {
    case 1:
      return syntaxTree(state);
    case 2:
      return syntaxTree(state).resolveInner(pos);
    case 3:
      return syntaxTree(state).resolveInner(pos, dir);
  }
}

console.log(tree(state, 0))
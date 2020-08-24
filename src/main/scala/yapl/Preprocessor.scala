package yapl

import yapl.language.{Global, Import}

import scala.io.{BufferedSource, Source}
import scala.util.Using

object Preprocessor {
    def importStdlib(): List[Global] = {
        val entry = Using(Source.fromResource(s"stdlib/stdlib.yapl")) {
            _.mkString
        }
        val (_, globals, imports) = Parser.parse(Lexer.lex(entry.get, s"stdlib/stdlib.yapl"))
        globals concat importModulesPrivate("stdlib/", imports, file => Source.fromResource(file))
    }

    def importModules(path: String, imports: List[Import]): List[Global] = {
        importModulesPrivate(path, imports, file => Source.fromFile(file))
    }

    private def importModulesPrivate(path: String, imports: List[Import], loadFunc: (String) => BufferedSource): List[Global] = {
        imports.foldLeft(Nil: List[Global])((result, imp) => {
            val program = Using(loadFunc(path + imp.file + ".yapl")) {
                _.mkString
            }

            if (program.isFailure) {
                println(s"Cannot find module ${imp.file}")
                return result
            }

            val tokens = Lexer.lex(program.get, imp.file)
            val (_, globals, imports) = Parser.parse(tokens)
            result concat globals concat importModules((path + imp.file).substring(0, (path + imp.file).lastIndexOf("/") + 1), imports)
        })
    }
}

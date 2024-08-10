module Main where

import System.IO
import Text.XHtml (content)


type NomeAutor = String
type Titulo = String
type Livro = (Titulo, NomeAutor, Bool)

--------------------------------------------------
--função para separar cada livro
leLivro :: String -> String
leLivro [] = []
leLivro (x:xs)
  |x /= '\n' = x: leLivro xs
---------------------------------------------------
separaInformaAux :: String -> String
separaInformaAux [] = error "Lista Vazia"
separaInformaAux (x:xs) 
  | x== ';' || x == ',' = x
  | otherwise = x:separaInformaAux xs

--função para separar as informações de cada livro
separaInforma :: String -> Livro
separaInforma [] = error "Lista Vazia"
separaInforma  (x:xs) 
  | x== ',' = (,_,)
  | x == ';' = (,x,)
  | x == '\n'
  | otherwise = separaInforma xs
  where 
---------------------------------------------------
meusLivros :: String -> [Livro]
meusLivros conteudo = separaInforma leLivro conteudo
---------------------------------------------------

----------------------------------------------------
--função para buscar livro
buscaLivro ::  [Livro] -> String -> [Livro]
buscaLivro [] _ = []
buscaLivro ((titulo, autor, emprestado): resto) nome
  | titulo == nome = (titulo, autor, emprestado) : buscaLivro resto nome
  | autor == nome = (titulo, autor, emprestado) : buscaLivro resto nome
  | otherwise = buscaLivro resto nome
---------------------------------------------------

main :: IO()
main = do
    conteudo <- readFile "meuarquivo.txt"
    putStrLn "-----SEJA BEM VINDO AO SIBLIB-----\n"
    putStrLn "|  O QUE DESEJA FAZER?           |\n"
    putStrLn "|  1 - BUSCA DE LIVRO            |\n"
    putStrLn "|  2 - EMPRESTIMO DE LIVRO       |\n"
    putStrLn "|  3 - DEVOLUCAO DE LIVRO        |\n"
    putStrLn "|  0 - SAIR                      |\n"
    putStrLn "----------------------------------"
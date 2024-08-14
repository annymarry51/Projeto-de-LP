module Main where

import System.IO

type NomeAutor = String
type Titulo = String
type Livro = (Titulo, NomeAutor, Bool)

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
   print ()
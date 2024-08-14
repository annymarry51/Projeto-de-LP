module Main where

import System.IO

type NomeAutor = String
type Titulo = String


-- Definição do tipo de dados Livro
data Livro = Livro {
    titulo :: Titulo,
    autor :: NomeAutor,
    emprestado :: Bool
} deriving Show
----------------------------------------------------------------
--livros que tem na biblioteca
livrosDaBiblioteca :: [Livro]
livrosDaBiblioteca = [
    Livro "O sertão vai virar mar" "Moacyr Scliar" True,
    Livro "Água viva" "Clarice Lispector" True,
    Livro "Terror na Festa" "Janaína Amado" True
    ]
----------------------------------------------------------------
--função para buscar livro
buscaLivro ::  [Livro] -> String -> [Livro]
buscaLivro [] _ = []
buscaLivro (livro: resto) nome
  | titulo livro == nome = livro : buscaLivro resto nome
  | autor livro == nome = livro : buscaLivro resto nome
  | otherwise = buscaLivro resto nome
---------------------------------------------------

main :: IO()
main = do
   print ()
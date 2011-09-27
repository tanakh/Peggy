---
title: The Parser Generator for Haskell
---

<div class="row">
<div class="span5">

## Peggyとは

PeggyはHaskell向けのパーザジェネレータです。
シンプルで扱いやすく、表現力のある
[Parsing expression grammer (PEG)][PEG]
を採用し、効率のよい[Packrat parser][PAC]を生成出来ます。

</div>

<div class="span4">

## 今すぐ始める

[Hackage][HAC]から、簡単にインストールできます。

    $ cabal update
    $ cabal install Peggy

[githubのレポジトリ][REPO]があります。
	
</div>

<div class="span5">

## 簡単・強力

[Peggyの構文](syntax.html) はとてもシンプルで、すぐに習得できます。
実用的なサーバをすぐに書き始められます。

</div>
</div>

-----

<div class="row">
<div class="span14">

## Parsing Expression Grammer

Peggy は文法として[Parsing Expression Grammer (PEG)][PEG]を採用しています。
PEGには次のような特徴があります。

- CFG (LL(1), LR(1), LALR(1), etc...) にある shift/reduce 競合が存在しない
- シンプルで表現力がある
- 無制限の先読みが可能、パーザとスキャナを分離する必要がない
- 入力長に対して線形時間での解析

PeggyはさらにPEGを拡張しています。

- sepBy拡張構文
- 強力なエラー検出とわかりやすいエラーメッセージの生成
- 左再帰のサポート

## モダンな設計

PeggyはモダンなHaskellコードを生成します。

- モナディック
- 高速な配列とハッシュマップの利用
- [Template Haskell][TH] と [Quasiquotation][QQ] のサポート
- 高速な string-like ([ByteString][BS], [Text][TXT]) シーケンスのサポート
- 扱いやすいエラーハンドリング

## 埋め込み DSL

Peggy はHaskellの embeded DSL (EDSL) として提供されます。
パーザをHaskellのソースコードに直接埋め込むことができます。
これはHaskellのコードを生成し、Haskellの型チェッカによって型検査されます。

    {-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
    
    import Text.Peggy
    
    [peggy|
    top :: Double = expr !.
    
    expr :: Double
      = expr "+" fact { $1 + $2 }
      / expr "-" fact { $1 - $2 }
      / fact
    
    fact :: Double
      = fact "*" term { $1 * $2 }
      / fact "/" term { $1 / $2 }
      / term
    
    term :: Double
      = "(" expr ")"
      / number
    
    number ::: Double
      = [1-9] [0-9]* { read ($1 : $2) }
    |]

    main :: IO ()
    main = print . parseString top "<stdin>" =<< getContents

## さあ、はじめよう！

利用を始めるために便利な、いくつかのドキュメントがあります。

- [チュートリアル](tutorial.html)
- [Peggyの構文のドキュメント](syntax.html)
- [Haddockのドキュメント](/haddock/index.html)

</div>
</div>

[PEG]: http://ja.wikipedia.org/wiki/%E8%A7%A3%E6%9E%90%E8%A1%A8%E7%8F%BE%E6%96%87%E6%B3%95
[PAC]: http://pdos.csail.mit.edu/~baford/packrat/
[HAC]: http://hackage.haskell.org/package/peggy
[REPO]: https://github.com/tanakh/peggy
[TH]: http://www.haskell.org/haskellwiki/Template_Haskell
[QQ]: http://www.haskell.org/haskellwiki/Quasiquotation
[BS]: http://hackage.haskell.org/package/bytestring
[TXT]: http://hackage.haskell.org/package/text

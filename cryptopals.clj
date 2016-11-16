( defn keybypow [pow]
  ( cond ( = 4 pow ) ( apply list (seq "0123456789abcdef" ) ) 
         ( = 6 pow ) ( apply list (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ) ) ) )

(defn keytree [ charset ] 
  ( let [ pivot (/ (count charset) 2) ] 
    ( if (< pivot 2) 
      charset
      (conj (map keytree
                 ( split-at pivot charset ) )
            ) ) ) ) 

( defn bittranslate 
  ( [ bits ] ( bittranslate bits (keybypow (reduce * (replicate 2 (count bits) ) ) ) ) )
  ( [ bits charset ]
  ( let [size ( count bits ) charset (keybypow size) ]
    ( loop [ branch ( keytree charset ) tmpbits bits ] 
      ( if ( seq? branch )
        ( recur ( nth branch (first tmpbits) )
                ( rest tmpbits ) ) 
        branch ) ) ) ) )

( defn bitarray [binval]
  ( loop [ iter binval ret '() ]
    ( if ( zero? iter )
      ret ( recur 
            ( bit-shift-right iter 1 )
            ( conj ret ( bit-and iter 1 ) ) ) ) ) )

( defn leftpad [c binval]
  ( concat (replicate ( - c ( count binval ) ) 0) binval ) )

( defn base2base [frompow topow input]
  ( ->> 
        ( flatten input )
        ( partition-all topow )
        ( map #(leftpad topow %) ) ) )

( defn strencode [pow input] 
  ( map #( ->> % 
                  ( .indexOf ( keybypow pow ) ) 
                  ( bitarray ) 
                  ( leftpad pow ) )
           ( seq input ) ) )

( defn strdecode [topow input] 
  ( let [ frompow ( count ( first input ) ) 
         flatinput (flatten input) ] 
    ( ->> input
          ( base2base frompow topow ) 
          ( map #(bittranslate %) ) 
          ( apply str ) ) ) )

( defn swapstrbase [frompow topow input] 
  ( let [ encoded ( strencode frompow input ) ]
    ( strdecode topow encoded ) ) ) 

( defn hex2b64 [input]
  ( str ( map #(get-in (bin2b64 6) %) (base2base 4 6 ) ) )

( defn xor_bit [a b] ( if ( = 1 ( + a b ) ) 1 0 ) )

( defn xor [a b] ( map xor_bit a b ) )

( defn hammer [a b] ( count ( filter #(not= true %) ( map = a b ) ) ) )

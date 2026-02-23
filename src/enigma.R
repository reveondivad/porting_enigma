# Enigma Machine — Rosetta Code Reference (R)
# Wehrmacht Enigma I: 3 rotors, Reflector B, plugboard.
# PeopleTec Inc. — Guinness World Record Attempt 2026

FWD <- c("EKMFLGDQVZNTOWYHXUSPAIBRCJ","AJDKSIRUXBLHWTMCQGZNPYFVOE","BDFHJLCPRTXVZNYEIWGAKMUSQO")
BWD <- c("UWYGADFPVZBECKMTHXSLRINQOJ","AJPCZWRLFBDKOTYUQGENHXMIVS","TAGBPCSDQEUFVNZHYIXJWLRKOM")
NOTCH <- c(81,69,86)  # Q, E, V
REFLECTOR <- "YRUHQSLDPXNGOKMIEBFZCWVJAT"

ci <- function(ch) utf8ToInt(ch) - 65L
cc <- function(i)  intToUtf8(i + 65L)

new_enigma <- function(rotors, key, plugboard=NULL) {
  e <- list(id=rotors+1L, off=ci(strsplit(key,"")[[1]]),
            plug=0:25)
  if (!is.null(plugboard)) {
    for (p in plugboard) {
      ab <- ci(strsplit(p,"")[[1]])
      e$plug[ab[1]+1] <- ab[2]; e$plug[ab[2]+1] <- ab[1]
    }
  }
  e
}

fwd_pass <- function(e, r, i) {
  c <- (i + e$off[r]) %% 26
  ch <- substr(FWD[e$id[r]], c+1, c+1)
  (ci(ch) - e$off[r] + 26) %% 26
}

bwd_pass <- function(e, r, i) {
  c <- (i + e$off[r]) %% 26
  ch <- substr(BWD[e$id[r]], c+1, c+1)
  (ci(ch) - e$off[r] + 26) %% 26
}

step_e <- function(e) {
  if (e$off[2] + 65 == NOTCH[e$id[2]]) {
    e$off[2] <- (e$off[2]+1) %% 26; e$off[1] <- (e$off[1]+1) %% 26
  } else if (e$off[3] + 65 == NOTCH[e$id[3]]) {
    e$off[2] <- (e$off[2]+1) %% 26
  }
  e$off[3] <- (e$off[3]+1) %% 26
  e
}

press_key <- function(e, ch) {
  e <- step_e(e)
  i <- ci(ch)
  i <- e$plug[i+1]
  i <- fwd_pass(e,3,i); i <- fwd_pass(e,2,i); i <- fwd_pass(e,1,i)
  i <- ci(substr(REFLECTOR, i+1, i+1))
  i <- bwd_pass(e,1,i); i <- bwd_pass(e,2,i); i <- bwd_pass(e,3,i)
  i <- e$plug[i+1]
  list(e=e, ch=cc(i))
}

encrypt <- function(e, text) {
  text <- gsub("[^A-Z]","",toupper(text))
  out <- ""
  for (j in seq_len(nchar(text))) {
    res <- press_key(e, substr(text,j,j))
    e <- res$e; out <- paste0(out, res$ch)
  }
  out
}

# Test vectors
tests <- list(
  list(c(0,1,2),"AAA",NULL,"AAAAA","BDZGO"),
  list(c(0,1,2),"AAA",NULL,"HELLOWORLD","ILBDAAMTAZ"),
  list(c(0,1,2),"AAA",NULL,"ATTACKATDAWN","BZHGNOCRRTCM"),
  list(c(0,1,2),"MCK",NULL,"HELLOWORLD","DLTBBQVPQV"),
  list(c(2,0,1),"AAA",NULL,"HELLOWORLD","KZHDFQYHXT"),
  list(c(0,1,2),"AAA",c("AB","CD","EF"),"HELLOWORLD","IKACBBMTBF")
)

all_ok <- TRUE
for (t in seq_along(tests)) {
  tv <- tests[[t]]
  e <- new_enigma(tv[[1]], tv[[2]], tv[[3]])
  ct <- encrypt(e, tv[[4]])
  ok <- ct == tv[[5]]
  cat(sprintf("Test %d: %-20s -> %-15s [%s]\n", t, tv[[4]], ct, ifelse(ok,"PASS","FAIL")))
  if (!ok) all_ok <- FALSE
}
cat(ifelse(all_ok, "\nALL 6 TESTS PASSED\n", "\nSOME TESTS FAILED\n"))

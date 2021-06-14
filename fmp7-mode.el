;;; Copyright (c) 2017-2021 Seiji Ohashi <sayzbrdg@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'generic-x)


;; generic mode
;;
(define-generic-mode fmp7-mode
  '(";")                                ; コメント開始文字列
  nil                                   ; キーワード
  ;; font-lock の設定
  '(("^'{" . font-lock-keyword-face)
    ("}" . font-lock-keyword-face)
    ("^'@" . font-lock-constant-face)
    ("^'[A-Z][0-9][A-Z0-9]*" . font-lock-function-name-face)
    ("^'%[[:space:]]+[a-zA-Z0-9]+" . font-lock-variable-name-face)
    ("%[a-zA-Z]" . font-lock-variable-name-face)
    ("[A-Za-z0-9]+=" . font-lock-builtin-face)
    ("\\\\v[bschti][+-]?" . font-lock-builtin-face)
    ("\\\\V[+-]?" . font-lock-keyword-face)
    ("\\\\[lmr][bschti]" . font-lock-type-face)
    ("[][/]" . font-lock-warning-face)
    )
  '("\\.mwi$")                          ; モードを有効にするファイル名
  '(fmp7-mode-setup)                     ; モード開始時に呼ばれる関数
  "FMP7 mode")


;; key map
;;
(defvar fmp7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'fmp7-save-and-compile-buffer)
    (define-key map "\C-c\C-p" 'fmp7-play-file)
    map)
  "fmp7-modeのキーマップ")


;; variables
;;
(defconst fmp7-default-file-extension ".owi"
  "PMDデータファイルのデフォルト拡張子")
(defconst fmp7-compilation-buffer-name "*fmp7-compilation*"
  "コンパイル結果を表示するバッファ名")

(defcustom fmp7-mode-hook nil
  "fmp7-modeのフック"
  :type '(hook))
(defcustom fmp7-after-compile-hook nil
  "コンパイルコマンド正常終了時のフック"
  :type '(hook))
(defcustom fmp7-compile-program-name "FMC7.exe"
  "FMP7コンパイラのプログラム名"
  :type '(string))
(defcustom fmp7-compile-program-options '()
  "FMP7コンパイラのコマンドラインオプション"
  :type '(repeat string))
(defcustom fmp7-player-program-name nil
  "FMP7ファイルプレイヤー"
  :type '(restricted-sexp :match-alternatives (stringp 'nil)))
(defcustom fmp7-player-program-options nil
  "FMP7ファイルプレイヤーのコマンドラインオプション"
  :type '(repeat string))
(defcustom fmp7-play-after-compile nil
  "コンパイル後に再生する場合はtを指定"
  :type '(boolean))
(defcustom fmp7-normalize-filename-function 'convert-standard-filename
  "ファイル名の正規化関数"
  :type '(restricted-sexp))


;; functions
;;
(defun fmp7-mode-setup ()
  (use-local-map fmp7-mode-map))


(defun fmp7-search-filename (&optional buffer)
  "バッファファイル名の拡張子を'fmp7-default-file-extension'に
置き換えたものを返す。バッファがファイルではない場合は nil を返す。
BUFFER が指定されなければ、カレントバッファを対象とする"
  (let ((basefilename (buffer-file-name buffer)))
    (and basefilename
         (concat (file-name-sans-extension basefilename)
                 fmp7-default-file-extension))))


(defun fmp7-play-file (&optional file)
  "FILEで指定されるコンパイル後のFMP7データを再生する。
FILEが指定されなければ、カレントバッファから推測される
ファイル名を使用する。'fmp7-player-program-name'が設定されて
いなかったり、FILEが指定されず、カレントバッファがファイル
ではない場合はエラーになる。コンパイル後のFMP7データファイルが
存在するかのチェックは行わない。"
  (interactive)
  (catch 'error
    (let ((filename (or file (fmp7-search-filename))))
      (unless fmp7-player-program-name
        (error "fmp7-player-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name))
        (throw 'error nil))
      ;; 起動した再生プロセスの終了を待つと、再生中の操作ができなくなる
      ;; ので待たない
      (apply 'call-process fmp7-player-program-name nil 0 nil
             (append fmp7-player-program-options
                     `(,(funcall fmp7-normalize-filename-function filename)))))))


(defun fmp7-compile-buffer-file (&optional buffer)
  "バッファのファイルを'fmp7-compile-program-name'でコンパイルする。
'fmp7-compile-program-name'が設定されていなかったり、バッファ
がファイルではない場合はエラーになる。事前にバッファの保存は
行わない。 BUFFER が指定されなければ、カレントバッファを
対象とする"
  (catch 'error
    (let ((filename (buffer-file-name buffer))
          (outbuffer (get-buffer-create fmp7-compilation-buffer-name)))
      (unless fmp7-compile-program-name
        (error "fmp7-compile-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name buffer))
        (throw 'error nil))
      (when (one-window-p)
        (split-window-vertically))
      ;; コンパイル結果出力先のバッファがウィンドウにあればそれを利用
      ;; 無ければ next-window を使用
      (set-window-buffer (or (get-buffer-window outbuffer)
                             (next-window))
                         outbuffer)
      (if (= 0 (with-current-buffer outbuffer
                 ;; コンパイル結果出力先のバッファは常に read only に
                 (let ((coding-system-for-read 'cp932-dos))
                   (unwind-protect
                       (progn
                         (setq buffer-read-only nil)
                         (erase-buffer)
                         (apply 'call-process fmp7-compile-program-name nil
                                outbuffer nil
                                (append fmp7-compile-program-options
                                        `(,(funcall fmp7-normalize-filename-function filename)))
                                ))
                     (setq buffer-read-only t)))))
          ;; コンパイル正常終了時はフックを実行
          (progn
            (run-hooks 'fmp7-after-compile-hook)
            t)
        nil))))


(defun fmp7-save-and-compile-buffer (&optional buffer)
  "バッファを保存しコンパイルする。指定があればその後再生する。
'fmp7-compile-program-name'が設定されていなかったり、バッファ
がファイルではない場合はエラーになる。
BUFFER が指定されなければ、カレントバッファを対象とする"
  (interactive)
  (catch 'error
    (let* ((targetbuffer (or buffer (current-buffer)))
           (filename (buffer-file-name targetbuffer)))
      (unless fmp7-compile-program-name
        (error "fmp7-compile-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name buffer))
        (throw 'error nil))
      (with-current-buffer targetbuffer
        (save-buffer))
      (when (and (fmp7-compile-buffer-file buffer) ; 異常時は継続しない
                 fmp7-play-after-compile)
        (fmp7-play-file (fmp7-search-filename buffer))))))


(provide 'fmp7-mode)

;; Local variables:
;; coding: utf-8
;; end:

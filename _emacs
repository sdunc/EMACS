;; Stephen Duncanson
;; stephen.duncanson@gmail.com
;; Personal Emacs configuration


;; god-mode
(god-mode)
(global-set-key (kbd "<escape>") #'god-mode-all) ; bind ESC to toggle god-mode
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(define-key god-local-mode-map (kbd ".") #'repeat)

(defun steph-god-mode-color-indicator ()
  "Toggles the color of the cursor and modeline to reflect whether god-mode is active."
  (let ((god-color "#f2bb4e")
	(normal-color "#d3ad8b"))
    (if god-local-mode
	(progn (set-cursor-color god-color) (set-face-background 'mode-line god-color))
      (progn (set-cursor-color normal-color) (set-face-background 'mode-line normal-color)))))

; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'steph-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'steph-god-mode-color-indicator)

;(global-hl-line-mode 1)


;; define startup message in scratch buffer
(setq initial-scratch-message "\
;; Welcome to Stephen's EMACS! 
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
")


;; movement changes
(setq scroll-error-top-bottom 'true)
(setq next-line-add-newlines t)


;; python on macOS, use homebrew install not 2.7
(setq python-shell-interpreter "/usr/local/bin/python3")


;; cosmetic 
(setq inhibit-startup-message t) ; Disable splash screen
(tool-bar-mode -1)   ; Remove toolbar
(setq visible-bell t)
(scroll-bar-mode 0)


;Line Numbers, except in certain major modes
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))
(global-display-line-numbers-mode)


;; Theme
(load-theme 'elmgrave t)


;; Package manager (MELPA)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package god-mode
	     :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aa857b381d069221db8aa095d7c7187c5e8e69823a190c31d1ba2916a2caf92c" "5bd300966277ce3de025ed420625c7ad19723627537bb882432ab656c973a7f5" "1d3552b31124b211d2be0095e0e0c2440357ce421a32504ba47dd4afe90defc8" "f69f285e0efbfaf2a8d001fa2089bc143eb0cc974ffdcf0f96c4e7fe88705d8a" "82844d85bb2512a607e9c366db31b89c8e225c51ab76c8a1d2eb8cc39b1abf98" "2fbf3590baa67fc5c0b5340da24146a8cf00e769044a5d76e0302fcc1f2c44b8" "6947040c9e0db0d14b975a856f1512514b1da0d59abf27a77820090e3a4aefa6" "5baf6e06069f6196db1876fb47a0501e41fdcaea79f2b7ded184050ac9bb9a42" "1c477470c46862084c9486c181b74be0d179e20dd40d41239f4a5302cde3863f" "9143d4637f092d051113e2a3732eefd1a17ee08d4fc88035917d35cbda4db870" "297b6036d84340e694bba74a755cdac5ebbee975514e847589d2845261b785ea" "ac6378ccb722cd01b7023d72f991339f548b2de3e62b64a455a89384d90f5bf9" "f592ad70810a3cc55ab7265ba0077c8853f816a81d786bfa062e9c8d17ec5907" "cc3c132454d204b638156d9fedb0dbc01168961f28b40085eeebe2fd41340f8c" "3ad78740df8827dda123ba5e7e71ecd0c29280c82fb11e8153cc769f8124d401" "f249c2dab4247043be7f0a2213bbe9c9657573971bb8be53c1323827abbe6720" "ffa39c99c13beeae0159b1f66a16e3cefa09a20ecfbb1c7bdb6f54b2f323e14b" "cf1fb1fc2da4ec08632aa7fcb57c3b8fa10652fcb3b7f591968a3a9824004fd2" "411f4f6cb49b98153ae21f4e0dd89af41244a289c2219850971771ece2009a0c" "cf2b49a12ed60b47c94295332fb574ca83f64566586cdb76473a86f6d3f35d6b" "a29780a9bab540693a119b320f667a5080ad63158194e0e75e24aa455edb5681" "dd42d711da8ea832cccdf9f6781d33840ced30ac6c44af0e8e9a8cfbaaaf4345" "7d1ab2251e2c3ce3619f2afba81d5b7492693e8b4fcd0fca9417ddc9a5e6943a" "1dc22346806c358e7b0ed3e3cc7f8fcf3b4ec921b871a9dadf64b975cd702c99" "93f38b864cdffb87c8c39f52d161b6977259c8338b4bddcca13faf486ff452fb" "55b0a1174fd56b762a4292380b852bc1be0dd0142699460090f17fe9b37b64de" "e47498520cfeb14dd12f2a0fb03cc4ac1fad2828effb98892a69d8dea4dbfe9a" "ef4679c1a8c3761c1534323b6f5b26c9bef1d73d4dab719e8eb2c8403be91e00" "c131707c7f585488546ab7d444f8513ff04e75e467d77adda0ca7b040dbfcfe8" "fa39a9d5c673e2562c668a5ddcdd56dd1c650fc93b3f51b81567eee018272cee" "0d6a0d214f97af8c066ea20887533cb945b9887ac520caaaa42406aff4e2827e" "8139c7621cb969f4ce273e290b39893aee835b18a52c1457c119e4ffdb072e4e" "835970789c0e521b0825a0af1ad2079790a58d0928f197b0dd7223a0b1c49c23" "64f3fcccd6a27b27bc655dfa6b35ebba04a044b2bb2d36e3970a39e2ad4dc02d" "e479ad2c50efee986da1e9ee21e3d610abbff7521aa34391fc8d8a2fd7e33d68" "7b99d199b37e68de50101617533762f1c9aff586aca60769c5087fc4b6681534" "4ad74737c60c737133e6f365ec46a0c120565076b057e4cc545cb3418232cf85" "5c5a7a3404fb9c0d8d4c19394abaeb0fbea9ee7ccc33a0f705ba5e433220f59c" "d49aa3f5e5f99622c0e504140f72d0252223c6f5225dc10a591519139aea22f1" "09d43d02fd8f3d337ceb43b1e978a01727c50ef113984d79b54b151a622a0080" "9463b29e8a7454ca68f5292d2a56e7b8656a1a0287ea7250aed457e3b1fc0c52" "579387c12e7f915645e57a097e55a241cbba6e4a693abbe6183f8a6b02cca915" "602616fb5cce5aa3efd18c74ceef82265908ab8d632da059453c96d9b4f8c82b" "71bc6078ce758de615144398a6225487133af0296c1901f1b28a24e135113ad3" "8bdc9b447f49553e39a793d1e924ba3bf80ef47e7181104bda0f0fdb4131163c" "10a4c151e0f682c6946cafaf78264f52f584e6a126aa4a2d665a6e2ca8f1dcfd" "7d833db8d39cbd1d6e8341dc020452ce6442da3c52668f6a1d42358c8308570c" "12c89638e3b6b02abf509a9b36de37371757f0423d67544533152568546e7642" "45ecd7f9000371e844533fbaf1e2abdc7569bdcdeff358ce6828f551f3fa9f99" "640bfe153647f61c00af7f4bfa6f29548b29cd599f8f25af904ed570ea621791" "ef3b66ccf0709bc742a8008e46bf9570f8c0aa327d354344f19b031c08e7acab" "b0cfc4202200c987c95992621270d09eb5ff649b17965578a7032f6c3d0dcb8f" "731b72511ca6b1f5255881d1611fc152ebe099177ecd16212607c0fdb96a3870" "0b7044783fcb451f829524b81cc3a284f4b5500f24d3dec7de22910fe928d264" "ec6a322c18af5dbc7e69c371a86cb55f3fb84cf76302266d89f139271ab527b9" "aced520f330073a999d70a6ea55bf08fcfddadda93f458ac200a0953624623a2" "8a6d6325e52f508f1dead9054057d079f9fbd0628965f0e4b1e354e36b38be43" "8a40c51df4adf98f2d89e8beb6a03a9eac7ef63810a4c1a16cce61a2d0f39f54" "669126cbd415130bf49b369e3366987b580e32a6ba72d126c2431bcb144911f9" "b1139513c9c6f8181b1c4e63e29f070950fab49801a27ceb535bb8cdf56a2f08" "e927d7d32ef130565372d1b1d653853717723f6ffbdbb541c33da0c54c196c40" "bf3b848debd3ebdb9a20158465278ab2cd93a31bfbe43f86f5c95abd5430c7b3" "243dfcd6cadf94dd0c195595a66fc6b9d9e66db6eca66a219b8079f3b0cad9dc" "3ebeed64dec98859eb50376081ff798faaa4386dc625f267147e7d583d36df9d" "449a96efbdefdb2a9a2ece2e219d19f88986ff9cacba82d7d5eec537fcc85bed" "e9f4428f7e6d6b5cbf134df983e5b61fe02c04e70faa328874036bd58291d756" "7b63bd3b803743c084869feda233ba8b18fee8746853abe6909067970879c716" "f78c6037e9fb1239201e3e45ecf773d860f8b43511ef31268d346f0bace6836d" "a686f59299a1f3d3a905bdb1771ae4a83a0ac9458e25ea851bf3a36e226d66cb" "23b8f243234ebed7edd21c6ff3dd612152967e6cc9ef63fd5b9a6fc8631fa3c7" "8d5844ea66b0703ebe7543cb3e07a4f7d099e5422aa07bb7a697dcc3efa4261e" "15444b9231d2f71ec5a2f49543f456bf85454641fd9d3e78ff6caf99deb1bc9d" "fafdb0580cc9f45bc60c5d0e4a1092f20d26f7b5102d47b2391cd42875923819" "f905a7e5c84184139167ece9abce7a18caa3a5bd06abce493b589b10c8438719" "4a7244b8a82bd7b725c3387309da46cce924721980bdc201720cd227ca234198" "1bb40f203b5b50c3efcb7d24222604d5b53963e8c5d5e20181717d694f3a22b7" "0e004b341de86b88c720dc8b318b630aa0aed88e482f70589474424c86ab572d" "8055f954f4834384b471f3a6338e8de32cad2631e43c04ea58f69c5949331be4" "8dbec8913f402e960deb55172bca26b827cb763b871eeb6fce4856512e3bc4b5" "aadb1156b72fdac5cbd34a35a4cb24beffc8b36ea7d8b577a3049a6332e889ae" "60afa5d9700c293824117fd2e657702182521203bfb89c763ccd23843ec59bad" "666819e119c6e5c5cedb910650634654a2439d45b5aa19d8fc5639b204085db8" "c6f0cbc2ac38ca1c0a6133152a07807a58b06a609cdc9be3f5c3cc35320227d6" "095e0712bdc6e269b040bc6351973e6de60a0e20750b740287d295a119803c19" "b10bb7365966ca8b51b4b7541180c7419aeaa0e0f033d19470336743a6eeca6f" "905cd813fc72d2a576234a584ba796addf4c0355ba90f98578503c9183e42ba2" "bf07e450b10cb4b81e616c691353503264727fec240ce6fa6247725c59f4322f" "513b44cd74c92162a853219cb7ee22858c50371ea61b438e07976ecb2999304e" "5de9679cc6cedaa55205dd564398270a3dcbb48d05efbdf12883946c54762656" "1c14fc23aa9f742004e2cc8294cc0e518f9257f5b4ff51268438f5428a013729" "2de0fc8a1e685619002f8f62eb190393c78eeaa6d66e30d60dfb006d5c7bc763" "743cd61a3eb74b3e8c91c06e4b64c67797178eb6371daaf687218ce9602853ce" "b108fba4135cd55b5ba0ed15e065217ef91458292118d3aa325920ff7971204b" "11a0a7d7b3ea9b07ff212df35d279f1f2a3828dfe69e593b55ddb3f5b946690c" "4b338ba0e6eeaefaebbe36c119724beed198ef96e4fb7fba1feb0a92fd339307" "b3ed6492f015cdccb9c3ad5bad4f239522c703e2c79807522d6bb3b1b1600302" "2dc37cf746143ca81f043e56f746a7321662d610eac78aba7afccaa8322ac190" "4bdb2124bf3163a7a7800a6b3cc304589847dbe094ff97107fe9b53703627ed3" "3b7500e9cc979f5af42f81d9c3d5d3397bbffb179f1bc6c7ce6740f5bcac7ceb" "e09aab93e5205b1c2fdff81fc292e5b0478551d0df8a11adfa9265bf95a9c8f3" "bb1a19d057a16b65b1d4fd3a946f93abc539312a43c106a603ff4dd202835900" "62f9c30c6f4e317dc3b965ce970e4eb09cace3d082f8856ee2a1e61be29ed41e" "b8900b5f335acb70e2bf0f11f4eb9bb5a0fa752a13357c58eff67f4972e25c3c" "50cac6071c4a995b5ea0cbcb72075b5cc16105a4a17a794279e1a37916cdc10c" "96718bc8c7cc2f5aac73bc9789f66941fc3a591ef0ab1e3f2b099906245192a0" "903674420d34623b9e4225e60e7cadc2908b5b92b31339d2722f814e1a8690fd" "0457df6434aa0dbe0383c0434637c34ffdb2aebf848b5c1dd9b299335550e50d" "340a0eab3c022693a99c6123b8d6919959a6d1c1a4b4e12f093218ffea15f358" "9db553cba1a1fa1757a16f440150812ffe987b758eee3686c09445d676f79dff" "d4d325f3f6f1626d8d3aa3a9b5f7a4a86fab7e6eba496f139ff1dea94d96ea0b" "d044817699ecdcadd650ad8f07114c335275f6e68269ccc1e916138575c3c856" "1222537b7ea856d922bf2ea6b101f26983da9c3f860a12958f2db30ce65bc023" "1cfe4c8411f84767959b7a7e3c6a15a43f8860aea748d5b55b75ecfdb475cedd" "592b8b27353cf8219d00a6979df995d45671d38f38128f676e8ff61259d76006" "c8e9965ed18b17883be59e5d1a05290b8e4e0f4697f077590eca96cf3ddf00bb" "ee38ee3fdfecb8d4cb3ce23d736de38a92a2d0bd0bafe9643db9c5b65d6449e7" "454b81c40be029b278339e7a2617a22222dcad7f1a147e851dcaffc920f2d5e7" "b61c78a90b808cc7bbcdbfcc6ef676a86e3cd32b6ce62df89f579008cc3e30a1" "25467f7a9ee26f4722771e5fb160c380b01f74e1014de86788498efaf01d4cb7" "c2ee43368eb69b46fa8f779302eddbece44dd30c5564c9f40f7f15b60e3ab611" "b7d6f24c4fab8efa2c67e1d45a5cd1c14790664c096606471989691ff1ed9c87" "c6567cc245e54c9626fdb295de39dac16891d3e6bc6dd2b71a44f6ebf4e2f8ea" "bab9dedaccd44a60df494cf201c30a156e4f8f556f4e780fcf1d95f220a64334" "753990daca536e014a5f1410afa9d29ad2f0c22c9996790aa9cc5a9b526d6754" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "a6473f7abf949f4a6a1a9cc0dd37ea2e35ba3cea65d3442b98d65c5c5c5cb8d7" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" default))
 '(disable-mouse-global-mode nil nil (disable-mouse))
 '(package-selected-packages
   '(rainbow-mode disable-mouse minimal-theme eink-theme basic-theme zenburn-theme use-package gruvbox-theme god-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

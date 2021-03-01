;; -*- emacs-lisp -*-

;; do not verify sign
(setq notmuch-crypto-process-mime nil)

;; searches
(setq notmuch-saved-searches
      (quote
       ((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
        (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first :search-type tree)
        (:name "linux-kernel" :query "to:linux-kernel@vger.kernel.org tag:unread" :key "l" :sort-order newest-first :search-type tree)
        (:name "kernelnewbies" :query "to:kernelnewbies@kernelnewbies.org tag:unread" :key "kn" :sort-order newest-first :search-type tree)
        (:name "org-mode" :query "to:emacs-orgmode@gnu.org tag:unread" :key "o" :sort-order newest-first :search-type tree)
        (:name "ceph" :query "to:*@ceph.io tag:unread" :key "c" :sort-order newest-first :search-type tree)
        (:name "mulang" :query "path:/Mulang/ tag:unread" :key "m" :sort-order newest-first :search-type tree)
        (:name "tracked" :query "tag:tracked" :key "t" :sort-order newest-first :search-type tree)
        (:name "later" :query "tag:later" :key "r" :sort-order newest-first)
        (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
        (:name "sent" :query "tag:sent" :key "s" :sort-order newest-first)
        (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first))))

(provide 'mail-cfg)

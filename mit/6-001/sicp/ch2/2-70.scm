###2.70###
PROMPT:
------------------------------------------------------------
Exercise 2.70.  The following eight-symbol alphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the ``symbols'' of an ``alphabet'' need not be individual letters.)

A	2	NA	16
BOOM	1	SHA	3
GET	2	YIP	9
JOB	2	WAH	1
Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree, and use encode (exercise 2.68) to encode the following message:

Get a job

Sha na na na na na na na na

Get a job

Sha na na na na na na na na

Wah yip yip yip yip yip yip yip yip yip

Sha boom

How many bits are required for the encoding? What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?
------------------------------------------------------------

(define message
  '(GET A JOB
	SHA NA NA NA NA NA NA NA NA
	GET A JOB SHA NA NA NA NA NA NA NA NA
	WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
	SHA BOOM))

(define tree (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (WAH 1) (BOOM 1))))
(define encoded-message (encode message tree))
;Value 17: (0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 1 1 0 0 1 1 0 1 1)

(length encoded-message)
;Value: 84

(decode encoded-message tree)
;Value 16: (get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom)


;;; 84 bits were required to encode the message with a variable length encoding. In a fixed-length encoding with 8 characters, 3 bits per character
;;; would be required (2^3). Since the message contains 36 characters, 108 bits would be required to encode the message.

module BookStore where
    data BookInfo = Book Int String [String]
                    deriving (Show)
    bookID      (Book id _     _      ) = id
    bookTitle   (Book _ title _      ) = title
    bookAuthors (Book _ _     authors) = authors

    data MagazineInfo = Magazine Int String [String]
                        deriving (Show)

    myInfo = Book 9780135072455 "Algebra of Programming"
             ["Richard Bird", "Oege de Moor"]

    data BookReview = BookReview BookInfo CustomerID ReviewBody
    type CustomerID = Int
    type ReviewBody = String

    type BookRecord = (BookInfo, BookReview)
    type CardHolder = String
    type CardNumber = String
    type Address    = [String]

    data BillingInfo = CreditCard CardNumber CardHolder Address
                     | CashOnDelivery
                     | Invoice CustomerID
                       deriving (Show)
   
    data Customer = Customer {
                             customerID :: CustomerID,
                             customerName :: String,
                             customerAddress :: Address
    } deriving (Show)

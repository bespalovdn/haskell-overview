module App.Data.TheType (
    AccessType(..),
    Array(..),
    BuiltinType(..),
    BT_Length(..),
    BT_Sign(..),
    BT_Type(..),
    ClassOfType(..),
    TheType(..),
    UserType(..),
    typeIsConst
    ) where

data TheType = TheType { 
    isConst::Maybe Int,
    classOfType::ClassOfType,
    accessType::Maybe AccessType,
    isArray::Maybe Array }

typeIsConst :: TheType -> Bool
typeIsConst (TheType Nothing _ _ _) = False
typeIsConst (TheType (Just _) _ _ _) = True

data ClassOfType = COT_BT BuiltinType | COT_UT UserType

data BuiltinType = BuiltinType { 
    btType::BT_Type, 
    btSignCount::Maybe (BT_Sign,Int), 
    btLength::Maybe BT_Length }

data BT_Type = BT_bool
             | BT_char
             | BT_double
             | BT_float
             | BT_int
             | BT_int8
             | BT_int16
             | BT_int32
             | BT_int64
             
data BT_Sign = BT_signed | BT_unsigned

data BT_Length = BT_long | BT_short

data UserType = UT_struct | UT_class

data AccessType = AT_ref { refConst::Maybe Int }
                | AT_ptr { ptrConst::Maybe Int, ptrToAT::Maybe AccessType }

instance Show BT_Length where
    show BT_long = "long"
    show BT_short = "short"

instance Show BT_Type where
    show BT_bool = "bool"
    show BT_char = "char"
    show BT_double = "double"
    show BT_float = "float"
    show BT_int = "int"
    show BT_int8 = "__int8"
    show BT_int16 = "__int16"
    show BT_int32 = "__int32"
    show BT_int64 = "__int64"

data Array = Array { arraySize::Maybe Int, subArray::Maybe Array }

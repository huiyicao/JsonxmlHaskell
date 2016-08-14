{-# LANGUAGE OverloadedStrings, RecordWildCards, ExtendedDefaultRules, DeriveGeneric, TypeFamilies, FlexibleInstances #-}

module JSON where

import Data.Aeson
import Data.Scientific
import Data.Maybe
import qualified Data.Text as T
import XmlRx.Types

instance FromJSON RecipeName where
    parseJSON (String v) = pure (RecipeName v) 
    parseJSON _ = fail "Invalid 'recipe_name' JSON"

instance FromJSON RecipePath where
    parseJSON (String v) = pure (RecipePath [(RecipeName v)]) 
    parseJSON _ = fail "Invalid 'recipe_path' JSON"

instance FromJSON XmlBool where
    parseJSON (Bool v) = pure (XmlBool v) 
    parseJSON _ = fail "Invalid 'xml_bool' JSON"

instance FromJSON XmlText where
    parseJSON (String v) = pure (XmlText v) 
    parseJSON _ = fail "Invalid 'xml_text' JSON"

instance FromJSON XmlDouble where
    parseJSON (Number v) = pure (XmlDouble $ realToFrac v) 
    parseJSON _ = fail "Invalid 'xml_double' JSON"

instance FromJSON RecipeStatus where
    parseJSON (String "Unknown")  = return Unknown
    parseJSON (String "Draft")    = return Draft
    parseJSON (String "Approved") = return Approved
    parseJSON (String "Review")   = return Review
    parseJSON _ = fail "Invalid 'recipe_status' JSON"

instance FromJSON RecipeType where
    parseJSON (String "Library") = return LibraryRecipe
    parseJSON (String "General") = return GeneralRecipe
    parseJSON (String "Site")    = return SiteRecipe
    parseJSON (String "Master")  = return MasterRecipe
    parseJSON _ = fail "Invalid 'recipe_type' JSON"

instance FromJSON SmilesString where
    parseJSON (String v) = pure (SmilesString v) 
    parseJSON _ = fail "Invalid 'smiles_string' JSON"


instance FromJSON MeasUnit where
    parseJSON (Object v) = 
        MeasUnit <$> v .:  "name"
                 <*> v .:  "symbol"
                 <*> v .:  "dimensionality"
    parseJSON _ = fail "Invalid 'measunit' JSON"

instance FromJSON InstrumentRole where
    parseJSON (Object v) = 
        InstrumentRole <$> v .:  "name"
                       <*> v .:? "description"
    parseJSON _ = fail "Invalid 'instrument_role' JSON"

instance FromJSON InstrumentType where
    parseJSON (Object v) =
        InstrumentType <$> v .:  "name"
                       <*> v .:  "code"
                       <*> v .:? "description"
                       <*> v .:  "role"
                       <*> v .:  "type"
    parseJSON _ = fail "Invalid 'instrument_type' JSON"

instance FromJSON Instrument where
    parseJSON (Object v) =
        Instrument <$> v .:  "name"
                   <*> v .:? "description"
                   <*> v .:? "type"
                   <*> v .:? "site"
                   <*> v .:? "location"
                   <*> v .:  "child"
    parseJSON _ = fail "Invalid 'instrument' JSON"

instance FromJSON AliasRole where
    parseJSON (String "name")              = return Name
    parseJSON (String "cas")               = return CAS
    parseJSON (String "mfcd")              = return MFCD
    parseJSON (String "roats_compound_id") = return RoatsCompoundID
    parseJSON (String "smiles")            = return SMILES
    parseJSON _ = fail "Invalid 'alias_role' JSON"
 
instance FromJSON Alias where
    parseJSON (Object v) =
        Alias <$> v .:  "name"
              <*> v .:  "role"
    parseJSON _ = fail "Invalid 'alias' JSON"

instance FromJSON Chemical where
    parseJSON (Object v) = 
        Chemical <$> v .:  "smiles"
                 <*> v .:? "formula"
                 <*> v .:? "mol_weight"
                 <*> v .:? "cas"
                 <*> v .:? "mfcd"
                 <*> v .:? "acc_num"
    parseJSON _ = fail "Invalid 'chemical' JSON"

instance FromJSON Catalog where
    parseJSON (Object v) = 
        Catalog <$> v .:  "number"
                <*> v .:  "alpha_num"
                <*> v .:? "name"
                <*> v .:? "label"
                <*> v .:? "vender_name"
                <*> v .:? "vender_contact"
    parseJSON _ = fail "Invalid 'catalog' JSON"

instance FromJSON IngredientRef where
    parseJSON (String v) = pure (IngredientRef (XmlText v))
    parseJSON _ = fail "Invalid 'ingredient_ref' JSON"

instance FromJSON Ingredient where
    parseJSON (Object v) = 
        Ingredient <$> v .:  "name"
                   <*> v .:  "consumed"
                   <*> v .:? "product"
                   <*> v .:? "material"
                   <*> v .:? "amount"
    parseJSON _ = fail "Invalid 'ingredient' JSON"

instance FromJSON MaterialRole where
    parseJSON (String "unknown")  = return UnknownMaterial
    parseJSON (String "hardware")    = return HardwareMaterial
    parseJSON (String "biological") = return BiologicalMaterial
    parseJSON (String "comsumable")   = return ConsumableMaterial
    parseJSON (String "chemical")   = return ChemicalMaterial
    parseJSON _ = fail "Invalid 'material_role' JSON"

instance FromJSON MaterialComponent where
    parseJSON (Object v) = 
        MaterialComponent <$> v .:  "percent"
                          <*> v .:  "matetial"
    parseJSON _ = fail "Invalid 'material_component' JSON"

instance FromJSON Material where
    parseJSON (Object v) = 
        Material <$> v .:  "name"
                 <*> v .:  "material_role"
                 <*> v .:  "alias"
                 <*> v .:? "chemical"
                 <*> v .:  "material_component"
    parseJSON _ = fail "Invalid 'material' JSON"

instance FromJSON Product where
    parseJSON (Object v) = 
        Product <$> v .:  "name"
                <*> v .:? "description"
                <*> v .:? "purity"
                <*> v .:? "low_purity"
                <*> v .:? "concentration"
                <*> v .:? "density"
                <*> v .:? "lot_size"
                <*> v .:? "quantity_string"
                <*> v .:? "price"
                <*> v .:? "currency"
                <*> v .:? "quantity"
                <*> v .:? "material"
                <*> v .:? "catalog"
    parseJSON _ = fail "Invalid 'product' JSON"

instance FromJSON Parameter where
    parseJSON (Object v) =
        Parameter <$> v .:  "name"
                  <*> v .:? "description"
                  <*> v .:  "version"
    parseJSON _ = fail "Invalid 'parameter' JSON"

instance FromJSON Location where
    parseJSON (Object v) =
        Location <$> v .:  "name"
                 <*> v .:  "type"
                 <*> v .:? "description"
                 <*> v .:? "site"
                 <*> v .:  "child"
    parseJSON _ = fail "Invalid 'location' JSON"

instance FromJSON Site where
    parseJSON (Object v) =
        Site <$> v .:  "name"
             <*> v .:? "address"
    parseJSON _ = fail "Invalid 'site' JSON"

instance FromJSON Person where
    parseJSON (Object v) =
        Person <$> v .:  "name"
               <*> v .:? "identifier"
               <*> v .:? "email"
               <*> v .:? "phone"
               <*> v .:? "site"
    parseJSON _ = fail "Invalid 'person' JSON"

instance FromJSON StepLimit where
    parseJSON (Object v) = do
        c <- v .:  "step_limit"
        case c of "string"    -> StepLimitString   <$> v .: "string_value"
                  "date_time" -> StepLimitDateTime <$> v .: "date_time_value"
                  "integer"   -> StepLimitInteger  <$> v .: "integer_value"
                  "real"      -> StepLimitReal     <$> v .: "real_value"
                  "external"  -> StepLimitExternal <$> v .: "external_value"
    parseJSON _ = fail "Invalid 'step_limit' JSON"

instance FromJSON StepArgAction where
    parseJSON (String "Input")  = return Input
    parseJSON (String "Output")    = return Output
    parseJSON (String "Input_Output") = return InputOutput
    parseJSON _ = fail "Invalid 'input_output' JSON"

instance FromJSON StepArg where
    parseJSON (Object v) = do
        c <- v .:  "type"
        case c of "step_ingredient" -> 
                        StepIngredient <$> v .:  "name"
                                       <*> v .:  "action"
                                       <*> v .:? "ingredient"
                                       <*> v .:? "amount"
                  "step_instrument" ->
                        StepInstrument <$> v .:  "name"
                                       <*> v .:  "action"
                                       <*> v .:? "instrument"
                                       <*> v .:? "instrument_type"
                                       <*> v .:? "role"
                  "step_parameter" ->
                        StepParameter  <$> v .:  "name"
                                       <*> v .:  "action"
                                       <*> v .:? "parameter"
                                       <*> v .:? "measunit"
                                       <*> v .:? "instrument_type"
                                       <*> v .:? "setpoint"
                                       <*> v .:? "lower_mor"
                                       <*> v .:? "lower_par"
                                       <*> v .:? "upper_mor"
                                       <*> v .:? "upper_par"
                                       <*> v .:  "parameter_ingredient"
                  _ -> fail "Invalid steparg"
    parseJSON _ = fail "Invalid 'step_args' JSON"

instance FromJSON StepPath where
    parseJSON (String v) = pure (StepPath [(StepName v)]) 
    parseJSON _ = fail "Invalid 'step_path' JSON"

instance FromJSON StepName where
    parseJSON (String v) = pure (StepName v) 
    parseJSON _ = fail "Invalid 'recipe_name' JSON"

instance FromJSON Step where
    parseJSON (Object v) = 
        Step <$> v .:  "name"
             <*> v .:  "description"
             <*> v .:  "path"
             <*> v .:? "stepargs"
             <*> v .:  "author"
             <*> v .:  "succ_step"
             <*> v .:  "pre_step"
    parseJSON _ = fail "Invalid 'step' JSON"

instance FromJSON Action where
    parseJSON (Object v) = 
        Action <$> v .:  "step"
    parseJSON _ = fail "Invalid 'action' JSON"

instance FromJSON Operation where
    parseJSON (Object v) = 
        Operation <$> v .:  "step"
                  <*> v .:  "action"
    parseJSON _ = fail "Invalid 'operation' JSON"

instance FromJSON Stage where
    parseJSON (Object v) = 
        Stage <$> v .:  "step"
              <*> v .:  "operation"
    parseJSON _ = fail "Invalid 'stage' JSON"

instance FromJSON Process where
    parseJSON (Object v) = 
        Process <$> v .:  "step"
                <*> v .:  "stage"
    parseJSON _ = fail "Invalid 'process' JSON"

instance FromJSON Recipe where
    parseJSON (Object v) = 
        Recipe <$> v .:  "name"
               <*> v .:  "description"
               <*> v .:  "status"
               <*> v .:? "effetive_data"
               <*> v .:  "version"
               <*> v .:  "path"
               <*> v .:  "type"
               <*> v .:? "product"
               <*> v .:? "material"
               <*> v .:? "site"
               <*> v .:  "author"
               <*> v .:  "approver"
               <*> v .:  "process"
               <*> v .:  "recipe"
    parseJSON _ = fail "Invalid 'recipe' JSON"



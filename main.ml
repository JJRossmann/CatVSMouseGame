open Types
open Fonctions
open Fonctions_jeu
open Graph
open Optimisation
open Random

let _ = Random.self_init ();;


let main () =
    (*finding_fifty standard_cat;;*)

    let cat_array, mouse_array = create_pool 13 121 121 15 225 in

    let cat_array = [| 
    {position= {x=50.000000; y=400.000000}; orientation= 0.000000; speed= 30.000000; reach= 30.000000; angle= 20.000000; heuristic= 6; weight_h= [| 0.61; 0.02; 0.37|]} ;
    {position= {x=50.000000; y=400.000000}; orientation= 0.000000; speed= 30.000000; reach= 30.000000; angle= 20.000000; heuristic= 6; weight_h= [| 0.65; 0.02; 0.33|]} ;
    {position= {x=50.000000; y=400.000000}; orientation= 0.000000; speed= 30.000000; reach= 30.000000; angle= 20.000000; heuristic= 4; weight_h= [| 0.97; 0.03; 0.00|]} ;
    {position= {x=50.000000; y=400.000000}; orientation= 0.000000; speed= 30.000000; reach= 30.000000; angle= 20.000000; heuristic= 4; weight_h= [| 0.98; 0.02; 0.00|]} 
    
    
    |] in

    (*let mouse_array = [| 
    {position= {x=750.000000; y=400.000000}; orientation= 180.000000; speed= 35.000000; reach= 20.000000; angle= 35.000000; heuristic= 9; weight_h= [| 0.50; 0.50; 0.|]};
    {position= {x=750.000000; y=400.000000}; orientation= 180.000000; speed= 35.000000; reach= 20.000000; angle= 35.000000; heuristic= 9; weight_h= [| 0.51; 0.49; 0.|]};
    {position= {x=750.000000; y=400.000000}; orientation= 180.000000; speed= 35.000000; reach= 20.000000; angle= 35.000000; heuristic= 10; weight_h= [| 0.56; 0.03; 0.41|]};
    {position= {x=750.000000; y=400.000000}; orientation= 180.000000; speed= 35.000000; reach= 20.000000; angle= 35.000000; heuristic= 10; weight_h= [| 0.58; 0.03; 0.39|]}

    |] in*)



    Printf.printf "%d cats VS %d mouses\n\n" (Array.length cat_array) (Array.length mouse_array);
    flush stdout;
    
    let _ = evolution cat_array mouse_array 100 2 0.025 in ();;

main ();;

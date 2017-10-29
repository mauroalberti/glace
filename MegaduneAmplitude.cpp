#include <cstdio>
#include <string>
#include <list>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <vector>


using namespace std;

class shot
{
    public:

        float x, y, z;

    private:

};

class Vector
{
    public:

        long double x, y, z;

        long double magnitude();
        Vector unitvector();

        Vector vectorbyscalar(Vector vector1, long double scalar1);
        Vector vectdiff(Vector vect2);
        Vector vector_projection(Vector vector1, Vector vector2);

        Vector antivector();
        Vector vectorproduct(Vector inputvector2);
        long double scalarproduct(Vector inputvector2);
        long double vectorsangle(Vector inputvector2);

    private:


    protected:


};

long double Vector::magnitude() // magnitude of vector
{
 return sqrt(x*x + y*y + z*z);
}

Vector Vector::unitvector() // normalization of vector
{

 Vector output_vector;

 long double magnit = magnitude();

 output_vector.x = x/magnit;
 output_vector.y = y/magnit;
 output_vector.z = z/magnit;

 return output_vector;
}

Vector Vector::antivector() // vector opposite
{
    Vector antivect;
    antivect.x = -x;
    antivect.y = -y;
    antivect.z = -z;

    return antivect;

}

long double Vector::scalarproduct(Vector inputvector2) // function scalarproduct
{
 long double x1 = x;
 long double y1 = y;
 long double z1 = z;

 long double x2 = inputvector2.x;
 long double y2 = inputvector2.y;
 long double z2 = inputvector2.z;

 //calculations
 return (x1*x2) + (y1*y2) + (z1*z2);

}


long double Vector::vectorsangle(Vector inputvector2) // function vectorsangle output in radians
{

 Vector inputvector1;

 inputvector1.x = x;
 inputvector1.y = y;
 inputvector1.z = z;

 if (inputvector1.magnitude() < 1.0e-5 || inputvector2.magnitude() < 1.0e-5) return -1.0;  // undetermined result, due to too small vector(s)

 long double cos_vectorsangle = inputvector1.scalarproduct(inputvector2)/(inputvector1.magnitude()*inputvector2.magnitude());

 long double vectorsangle_rad;

 if (cos_vectorsangle > 1.0) vectorsangle_rad = 0.0;
 else if  (cos_vectorsangle < -1.0) vectorsangle_rad = M_PI;
 else vectorsangle_rad = acos(cos_vectorsangle);

 return vectorsangle_rad;

}


int main ()
{

    int i,j;

    float delta_thresh_incr = 0.05;

    /* definizione del file di input */
    cout << "Enter input file name: ";
  	string filename;
    cin >> filename;

    ifstream infile;
    infile.open(filename.c_str(), ios::binary);

    if (infile.fail())
    {
    cout << "File di input " << filename << " non trovato\n";
    return 1;
    }
    else
    {
    cout << "File di input riconosciuto e aperto\n";
    }


    // definizione del file di output puntuale
    cout << "Enter output point file name: ";
  	string output_pointfilename;
    cin >> output_pointfilename;

    ofstream outpointfile;
    outpointfile.open(output_pointfilename.c_str(), ios::binary);

    if (outpointfile.fail())
    {
    cout << "File di output " << output_pointfilename << " non creato\n";
    return 1;
    }
    else
    {
    cout << "File di output riconosciuto e aperto\n";
    }

    outpointfile << "track,minmax,x,y,z,lambda,dz_min,slope_app,dz_maxtrend\n";  // scrive header file


    // definizione radice nome file di output lineare
    cout << "Enter output line file name: ";
  	string output_linefilename;
    cin >> output_linefilename;

    // crea file con le info spaziali - formato GENERATE
  	string output_linefilename_gen = output_linefilename+".gen";
    ofstream outlinefile_sp;
    outlinefile_sp.open(output_linefilename_gen.c_str(), ios::binary);

    // crea file con le info attributi - formato per ArcView
  	string output_linefilename_att = output_linefilename+".txt";
    ofstream outlinefile_at;
    outlinefile_at.open(output_linefilename_att.c_str(), ios::binary);

    // gestione in-successo creazione file di output lineari
    if (outlinefile_sp.fail() || outlinefile_at.fail())
    {
    cout << "Files di output lineare " << output_linefilename << " non creato/i\n";
    return 1;
    }
    else
    {
    cout << "Files di output lineare creati\n";
    }

    outlinefile_at << "id,track,type,length,slope,dz,lenght_rt,slope_rt\n";  // scrive header file per file con attributi

    // definizione del threshold minimo di differenza altezza
    cout << "differenza minima nella discrepanza di elevazione tra massimo e trend minimi: ";
    float delta_height_minthresh;
    cin >> delta_height_minthresh;


    // definizione della direzione di ricerca e tolleranza angolare

    cout << "angolo di ricerca: ";
  	int angle_searchdir_deg;
    cin >> angle_searchdir_deg;

    cout << "tolleranza angolare nella ricerca: ";
  	int tolerangle_searchdir_deg;
    cin >> tolerangle_searchdir_deg;

    float tolerangle_searchdir_rad = tolerangle_searchdir_deg*M_PI/180.0;

    Vector SearchDirection_Vector;
    SearchDirection_Vector.x = sin (angle_searchdir_deg*M_PI/180.0);
    SearchDirection_Vector.y = cos (angle_searchdir_deg*M_PI/180.0);
    SearchDirection_Vector.z = 0;

    Vector SearchDirection_AntiVector = SearchDirection_Vector.antivector();

    //
    // inizio processamento input dati
    //



    float dz1,dz2,dz3;


    shot new_shot, locminshot0, locminshot1, locmaxshot;

    int id_lineelem = 0;


    int numrecs_track = 0;
    bool firstminimumoftrack_found = false;

    int idtrack_curr = 0, idtrack;

    // inizializzazione dei valori nello shot array
    shot ShotsArray[3];
    ShotsArray[0].x = 0; ShotsArray[0].y = 0;  ShotsArray[0].z = 0;
    ShotsArray[1].x = 0; ShotsArray[1].y = 0;  ShotsArray[1].z = 0;
    ShotsArray[2].x = 0; ShotsArray[2].y = 0;  ShotsArray[2].z = 0;

    int track_proc_case;

    string rec_line;

    // legge l'header del file
    getline(infile, rec_line);

    while (!infile.eof())
    {

        // read record
        getline(infile, rec_line);
        istringstream instr(rec_line);
        instr >> idtrack >> new_shot.x >> new_shot.y >> new_shot.z;
        cout << idtrack << new_shot.x << " " << new_shot.y << " " << new_shot.z << "\n";

        // update values in array - elevation change
        ShotsArray[0].x = ShotsArray[1].x; ShotsArray[0].y = ShotsArray[1].y;  ShotsArray[0].z = ShotsArray[1].z;
        ShotsArray[1].x = ShotsArray[2].x; ShotsArray[1].y = ShotsArray[2].y;  ShotsArray[1].z = ShotsArray[2].z;
        ShotsArray[2].x = new_shot.x; ShotsArray[2].y = new_shot.y;  ShotsArray[2].z = new_shot.z;

        // define track processing
        if (idtrack != idtrack_curr)
        {
            track_proc_case = 0;
        }
        else if (!firstminimumoftrack_found)
        {
            track_proc_case = 1;
        }
        else
        {
            track_proc_case = 2;
        }


        Vector FirstMinimum;

        switch(track_proc_case)
        {
            case 0:

                idtrack_curr = idtrack;
                firstminimumoftrack_found = false;

                FirstMinimum.x = new_shot.x; FirstMinimum.y = new_shot.y; FirstMinimum.z = new_shot.z;


                break;


            case 1:

                if (new_shot.z <= FirstMinimum.z)
                {
                    FirstMinimum.x = new_shot.x; FirstMinimum.y = new_shot.y; FirstMinimum.z = new_shot.z;
                }
                else
                {
                    firstminimumoftrack_found = true;
                    locminshot0.x = FirstMinimum.x; locminshot0.y = FirstMinimum.y; locminshot0.z = FirstMinimum.z;
                    locmaxshot.x = new_shot.x; locmaxshot.y = new_shot.y; locmaxshot.z = new_shot.z;
                }

                break;


            case 2:

                if (new_shot.z >= locmaxshot.z)
                {
                    locmaxshot.x = new_shot.x;
                    locmaxshot.y = new_shot.y;
                    locmaxshot.z = new_shot.z;
                }

                else

                {


                    dz2 = ShotsArray[2].z - ShotsArray[1].z;
                    dz1 = ShotsArray[0].z - ShotsArray[1].z;

                     //trovato minimo
                    if (dz1 > delta_thresh_incr && dz2 > delta_thresh_incr)
                    {

                        locminshot1.x = ShotsArray[1].x;
                        locminshot1.y = ShotsArray[1].y;
                        locminshot1.z = ShotsArray[1].z;

                        // calcola lunghezza d'onda apparente e slope apparente
                        float lambda_x = locminshot1.x - locminshot0.x;
                        float lambda_y = locminshot1.y - locminshot0.y;
                        float lambda = sqrt((lambda_x*lambda_x) + (lambda_y*lambda_y));
                        float dz_mins = locminshot1.z - locminshot0.z;
                        float slope_app = dz_mins/lambda;

                        // definisce vettore parallelo all'orientazione locale della traccia, per successivo test di compatibilità con la direzione di ricerca
                        Vector TrackDirection_Vector;
                        TrackDirection_Vector.x = lambda_x;
                        TrackDirection_Vector.y = lambda_y;
                        TrackDirection_Vector.z = 0;

                        //interpola valore trend al punto corrispondente al massimo locale trovato
                        float dist_min0max_x = locmaxshot.x - locminshot0.x;
                        float dist_min0max_y = locmaxshot.y - locminshot0.y;
                        float dist_min0max = sqrt(dist_min0max_x*dist_min0max_x + dist_min0max_y*dist_min0max_y);
                        float trend_zmax = locminshot0.z + (slope_app*dist_min0max);

                        // calcola differenza tra trend interpolato e max
                        float dz_trendmax = locmaxshot.z - trend_zmax;

                        // calcola estensione e slope delle facce della struttura
                        float estens_faceA = dist_min0max;
                        float estens_faceB = lambda - estens_faceA;

                        float dz_faceA = locmaxshot.z - locminshot0.z;
                        float slope_app_faceA = dz_faceA/estens_faceA;

                        float dz_faceB = locmaxshot.z - locminshot1.z;
                        float slope_app_faceB = dz_faceB/estens_faceB;

                        float estens_largeface = estens_faceA; float slope_app_largeface = slope_app_faceA;
                        float estens_shortface = estens_faceB; float slope_app_shortface = slope_app_faceB;
                        Vector LargeFace_MinPoint, ShortFace_MinPoint;
                        LargeFace_MinPoint.x = locminshot0.x; LargeFace_MinPoint.y = locminshot0.y; LargeFace_MinPoint.z = locminshot0.z;
                        ShortFace_MinPoint.x = locminshot1.x; ShortFace_MinPoint.y = locminshot1.y; ShortFace_MinPoint.z = locminshot1.z;
                        if (estens_faceB > estens_faceA)
                        {
                            estens_largeface = estens_faceB; slope_app_largeface = slope_app_faceB;
                            estens_shortface = estens_faceA; slope_app_shortface = slope_app_faceA;
                            LargeFace_MinPoint.x = locminshot1.x; LargeFace_MinPoint.y = locminshot1.y; LargeFace_MinPoint.z = locminshot1.z;
                            ShortFace_MinPoint.x = locminshot0.x; ShortFace_MinPoint.y = locminshot0.y; ShortFace_MinPoint.z = locminshot0.z;
                        }

                        float ratio_estensFL_FS = estens_largeface / estens_shortface;
                        float ratio_slopeFL_FS = slope_app_largeface / slope_app_shortface;

                        if (dz_trendmax > delta_height_minthresh)
                        {

                            if (TrackDirection_Vector.vectorsangle(SearchDirection_Vector) <= tolerangle_searchdir_rad ||
                                TrackDirection_Vector.vectorsangle(SearchDirection_AntiVector) <= tolerangle_searchdir_rad)
                                {

                                    //output di nuovo minimo locale
                                    // con valori descrittivi
                                    outpointfile << idtrack_curr << ",min," << locminshot0.x << "," <<
                                    locminshot0.y << "," << locminshot0.z << ","
                                    << lambda << "," << dz_mins << "," << slope_app << ",\n";

                                    //output di massimo locale
                                    // con valori descrittivi
                                    outpointfile << idtrack_curr << ",max," << locmaxshot.x << "," <<
                                    locmaxshot.y << "," << locmaxshot.z << ",,,,"
                                    << dz_trendmax << "\n";

                                     //output per tema lineare
                                    // segmento complessivo
                                     id_lineelem++;
                                        // info spaziali
                                     outlinefile_sp << id_lineelem << "\n" <<
                                        locminshot0.x << "," << locminshot0.y << "\n" <<
                                        locminshot1.x << "," << locminshot1.y << "\n" <<
                                        "END" << "\n";
                                        // info tabellari
                                     outlinefile_at << id_lineelem << "," << idtrack_curr << ",full," <<
                                         lambda << "," << abs(slope_app) << "," << abs(dz_mins) << "," <<
                                         ratio_estensFL_FS << "," << ratio_slopeFL_FS << "\n";

                                    // segmento breve
                                     id_lineelem++;
                                        // info spaziali
                                     outlinefile_sp << id_lineelem << "\n" <<
                                        locmaxshot.x << "," << locmaxshot.y << "\n" <<
                                        ShortFace_MinPoint.x << "," << ShortFace_MinPoint.y << "\n" <<
                                        "END" << "\n";
                                        // info tabellari
                                     outlinefile_at << id_lineelem << "," << idtrack_curr << ",short," <<
                                         estens_shortface << "," << slope_app_shortface << "," << locmaxshot.z-ShortFace_MinPoint.z << ",,\n";


                                    // segmento lungo
                                     id_lineelem++;
                                        // info spaziali
                                     outlinefile_sp << id_lineelem << "\n" <<
                                        locmaxshot.x << "," << locmaxshot.y << "\n" <<
                                        LargeFace_MinPoint.x << "," << LargeFace_MinPoint.y << "\n" <<
                                        "END" << "\n";
                                        // info tabellari
                                     outlinefile_at << id_lineelem << "," << idtrack_curr << ",long," <<
                                         estens_largeface << "," << slope_app_largeface << "," << locmaxshot.z-LargeFace_MinPoint.z << ",,\n";

                                }

                            // aggiorna nuovo minimo locale
                            locminshot0.x = locminshot1.x;
                            locminshot0.y = locminshot1.y;
                            locminshot0.z = locminshot1.z;

                            // aggiorna nuovo massimo locale
                            locmaxshot.x = locminshot1.x;
                            locmaxshot.y = locminshot1.y;
                            locmaxshot.z = locminshot1.z;

                        }

                    }


                }

                break;

        }

    }

    outlinefile_sp << "END" << "\n";

    infile.close();

    outpointfile.close();
    outlinefile_sp.close();
    outlinefile_at.close();

    return 0;

}






/*
#
# minmax_04c.cpp
# by Mauro Alberti - www.malg.eu, alberti.m65@gmail.com
# 2010-09-16
#
# This program or module is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 2 of the License, or
# version 3 of the License, or (at your option) any later version. It is
# provided for educational purposes and is distributed in the hope that
# it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
# the GNU General Public License for more details.
#
*/

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

class minmax_shotresult
{

    public:

        char minmax_type;
        shot mainmaxshot;

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


string process_track(int track_id, int track_totrecnum, shot ShotsArray[])
{

    ostringstream ostreamstring;

    ShotsArray[track_totrecnum];

    minmax_shotresult minmax_result;

    for (int rec_cnt = 0; rec_cnt < track_totrecnum-4; rec_cnt++)
    {


        float delta_1 = ShotsArray[rec_cnt+1].z - ShotsArray[rec_cnt].z;
        float delta_2 = ShotsArray[rec_cnt+2].z - ShotsArray[rec_cnt+1].z;
        float delta_3 = ShotsArray[rec_cnt+3].z - ShotsArray[rec_cnt+2].z;
        float delta_4 = ShotsArray[rec_cnt+4].z - ShotsArray[rec_cnt+3].z;

        char minmax_type;

        if (delta_1 >= 0.0 && delta_2 >= 0.0 && delta_3 <= 0.0 && delta_4 <= 0.0 )
        {

                //  outputta lo shot con la max discrepanza assoluta
                ostreamstring << track_id << "," << ShotsArray[rec_cnt+2].x << "," << ShotsArray[rec_cnt+2].y
                    << "," << ShotsArray[rec_cnt+2].z << "," << "max" << "\n";

        }

        if (delta_1 <= 0.0 && delta_2 <= 0.0 && delta_3 >= 0.0 && delta_4 >= 0.0 )
        {
                //  outputta lo shot con la max discrepanza assoluta
                ostreamstring << track_id << "," << ShotsArray[rec_cnt+2].x << "," << ShotsArray[rec_cnt+2].y
                    << "," << ShotsArray[rec_cnt+2].z << "," << "min" << "\n";
        }


    }


    string track_result_string = ostreamstring.str();


    return track_result_string;
}

int main ()
{

    int i,j;

    cout << "\n\nminmax_04c\n";
    cout << "www.malg.eu\n";
    cout << "2010-09-16\n\n\n";


    /* definizione del file di input */
    cout << "Enter input file name: ";
  	string filename;
    cin >> filename;

    ifstream infile;
    infile.open(filename.c_str(), ios::binary);

    if (infile.fail())
    {
    cout << "Input file " << filename << " not found\n";
    return 1;
    }


    // definizione del file di output puntuale
    cout << "Enter output file name: ";
  	string output_pointfilename;
    cin >> output_pointfilename;

    ofstream outpointfile;
    outpointfile.open(output_pointfilename.c_str(), ios::binary);

    if (outpointfile.fail())
    {
    cout << "Unable to create output file " << output_pointfilename << "\n";
    return 1;
    }


    outpointfile << "track,x,y,z,minmax\n";  // scrive header file



    // definizione della direzione di ricerca e tolleranza angolare

    cout << "trend angle: ";
  	int angle_searchdir_deg;
    cin >> angle_searchdir_deg;

    cout << "angular tolerance: ";
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


    shot new_shot;

    int numrecs_track = 0;

    int idtrack;

    string rec_line;

    int num_recs_read = 0;
    int idtrack_curr = 0;

    int track_currrecnum, track_totrecnum;

    string track_result_string;

    // shot array
    shot ShotsArray[5000];



    // legge l'header del file
    getline(infile, rec_line);


    while (!infile.eof())
    {

        // read record
        getline(infile, rec_line);
        istringstream instr(rec_line);
        instr >> idtrack >> new_shot.x >> new_shot.y >> new_shot.z;

        num_recs_read++;

        if (num_recs_read == 1)
        {

         // inizializza valore del numero di dati nella traccia corrente
         track_currrecnum = 0;

         // vi scrive record corrente
        ShotsArray[track_currrecnum].x = new_shot.x; ShotsArray[track_currrecnum].y = new_shot.y;  ShotsArray[track_currrecnum].z = new_shot.z;

         // continua nella lettura

        }
        else if (idtrack != idtrack_curr)

        {

        // fissa il valore del numero di dati nella traccia completata NOTA: N-1
        track_totrecnum = track_currrecnum;


        // calcola direzione track
        Vector TrackDirection_Vector;
        TrackDirection_Vector.x = ShotsArray[track_totrecnum].x - ShotsArray[0].x;
        TrackDirection_Vector.y = ShotsArray[track_totrecnum].y - ShotsArray[0].y;
        TrackDirection_Vector.z = 0;


        if (TrackDirection_Vector.vectorsangle(SearchDirection_Vector) <= tolerangle_searchdir_rad ||
            TrackDirection_Vector.vectorsangle(SearchDirection_AntiVector) <= tolerangle_searchdir_rad)
            {



                 // processa i valori della traccia per trovare i minimi ed i massimi
                track_result_string = process_track(idtrack_curr, track_totrecnum, ShotsArray);


                cout << "result: " << track_result_string;



                // scrive nel file di output i risultati
                outpointfile << track_result_string;


            }


         // re-inizializza identificatore traccia corrente
         idtrack_curr = idtrack;


         // re-inizializza valore del numero di dati nella traccia corrente
         track_currrecnum = 0;


         // vi scrive record corrente
        ShotsArray[track_currrecnum].x = new_shot.x; ShotsArray[track_currrecnum].y = new_shot.y;  ShotsArray[track_currrecnum].z = new_shot.z;


         // continua nella lettura


        }

        else

        {

        // incrementa il numero di records presenti nella singola traccia
        track_currrecnum++;

        // aggiunge all'array corrente i valori del record corrente
        ShotsArray[track_currrecnum].x = new_shot.x; ShotsArray[track_currrecnum].y = new_shot.y;  ShotsArray[track_currrecnum].z = new_shot.z;

         // continua nella lettura

        }



    }

    // processa i valori dell'ultimo array non completato


    infile.close();

    outpointfile.close();

    return 0;

}




